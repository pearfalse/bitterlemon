//! Handles encoding of a Bitterlemon data stream.
// I/O variant, 0.3.x

use crate::{
	encode::Run,
	MAX_RUN_SIZE,
	MAX_FRAME_SIZE,
};

use std::mem::replace;

#[derive(Debug)]
struct RunBuilder {
	current: Option<Run>,
}

impl RunBuilder {
	pub fn new() -> RunBuilder {
		RunBuilder {
			current: None,
		}
	}

	pub(crate) fn update(&mut self, bit: bool) -> Option<Run> {
		match self.current.as_mut() {
			Some(tail) if tail.len() < MAX_RUN_SIZE && tail.bit() == bit => {
				tail.increment();
				None
			},
			_ => {
				// fresh run
				replace(&mut self.current, Some(Run::new(bit)))
			},
		}
	}

	pub(crate) fn flush(self) -> Option<Run> {
		let mut this = self;
		this.current.take()
	}
}

#[cfg(test)]
mod test_run_builder {
	use super::*;

	#[test]
	fn one_run() {
		let mut encoder = RunBuilder::new();
		const LEN: u8 = 64;
		(0..LEN).for_each(|_| assert_eq!(None, encoder.update(true)));

		assert_eq!(Some(Run::Set(LEN)), encoder.flush());
	}

	#[test]
	fn two_runs() {
		let mut encoder = RunBuilder::new();
		const LEN: u8 = 96;
		(0..MAX_RUN_SIZE).for_each(|_| assert_eq!(None, encoder.update(true)));

		// 65th run
		assert_eq!(Some(Run::Set(MAX_RUN_SIZE).into()), encoder.update(true));

		(MAX_RUN_SIZE+1 .. LEN).for_each(|_| assert_eq!(None, encoder.update(true)));
		assert_eq!(Some(Run::Set(LEN - MAX_RUN_SIZE).into()), encoder.flush());
	}

	#[test]
	fn alternate() {
		let mut encoder = RunBuilder::new();

		assert_eq!(None, encoder.update(true));

		for _ in 0..100 {
			assert_eq!(Some(Run::Set(1)), encoder.update(false));
			assert_eq!(Some(Run::Clear(1)), encoder.update(true));
		}

		assert_eq!(Some(Run::Set(1)), encoder.flush());
	}
}

const STAGE_SIZE: usize = (MAX_FRAME_SIZE / 8) as usize;


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
enum Bit {
	Bit0 = 0,
	Bit1 = 1,
	Bit2 = 2,
	Bit3 = 3,
	Bit4 = 4,
	Bit5 = 5,
	Bit6 = 6,
	Bit7 = 7,
}

impl Bit {
	fn inc(self) -> (Self, bool) {
		use Bit::*;
		if self == Bit7 {
			(Bit0, true)
		} else {
			(unsafe {
				// OOB case was checked above
				std::mem::transmute((self as u8).wrapping_add(1))
			}, false)
		}
	}

	fn dec(self) -> (Self, bool) {
		use Bit::*;
		if self == Bit0 {
			(Bit7, true)
		} else {
			(unsafe {
				// OOB case was checked above
				std::mem::transmute((self as u8).wrapping_sub(1))
			}, false)
		}
	}
}

impl std::ops::Deref for Bit {
	type Target = u8;
	fn deref(&self) -> &Self::Target {
		// u8 is a strict superset of Self, and we don't impl DerefMut
		unsafe { std::mem::transmute(self) }
	}
}

#[derive(Debug)]
struct FrameBuilder {
	// assemble frame here
	frame_stage: [u8; STAGE_SIZE],

	// index of frame byte to add to
	frame_idx: u8,

	// flushing edge case
	frame_single_run: Option<Run>,

	// bit # (0–7) to add to next
	stage_bit: Bit,

	// if Some, continue flushing frame from this idx
	flush_idx: Option<u8>,
}

const TOP_BIT: u8 = 7;
const BOTTOM_BIT: u8 = 1;

impl FrameBuilder {
	pub fn new() -> FrameBuilder {
		FrameBuilder {
			frame_stage: [0; STAGE_SIZE],
			frame_idx: 0,
			frame_single_run: None,
			stage_bit: Bit::Bit0, // BL is LSB-first from 0.3
			flush_idx: None,
		}
	}

	fn flush_bit_count(&self) -> u8 {
		let whole_bytes = self.frame_idx * 8;
		let partial = self.stage_bit as u8;
		whole_bytes + partial
	}

	pub fn update(&mut self, run: &mut Option<Run>) -> Option<u8> {
		let mut flush_now = false;
		let flush_bit_count = self.flush_bit_count();

		let mut add_run_to_frame = None;
		if let Some(len) = run.map(|r| r.len()) {
			// we were given a run, and we have its length
			if self.flush_idx.is_none() && (|| {
				// there's a run we can pull, and a frame we're assembling
				// to add or not to add? the options are:
				// - pad the frame and output a run (cost: padding + 8)
				// - add to the frame (cost: run size)
				// when equal, add to frame -- mitigate pathological cases where frames are
				// consistently opened after balanced runs close them

				if len >= 16 { return false; } // too big to benefit
				if len < 8 { return true; } // always beneficial

				// would the frame get too large?
				if flush_bit_count + len > MAX_FRAME_SIZE {
					// start emptying frame
					flush_now = true;
					return false;
				}

				let padding = (8 - self.stage_bit as u8) & 7;
				padding + 8 >= len
			})() {
				add_run_to_frame = run.take(); // will be Some()
			}
		}

		if flush_now {
			// start flushing this frame; we'll output its header now
			self.flush_idx = Some(0);
			self.stage_bit = Bit::Bit0;
			return Some(match flush_bit_count {
				n if n == MAX_FRAME_SIZE => 0,
				n => n,
			});
		}

		if let Some(ref mut fi) = self.flush_idx {
			// flush byte of frame
			let r = self.frame_stage[*fi as usize];
			*fi += 1;
			if *fi == self.frame_idx {
				// frame completely sent; reset
				self.flush_idx = None;
			}
			return Some(r);
		}

		if let Some(run) = add_run_to_frame {
			// init frame
			self.pour_run_into_frame(run);
			None
		}
		else {
			// pump run out
			run.take().map(u8::from)
		}
	}

	fn pour_run_into_frame(&mut self, run: Run) {
		let bit = run.bit() as u8;
		for _ in 0..run.len() {
			let tgt = &mut self.frame_stage[self.frame_idx as usize];
			*tgt |= bit << *self.stage_bit;

			// increment bit
			let (stage_bit, wrap_idx) = self.stage_bit.inc();
			self.stage_bit = stage_bit;
			if wrap_idx {
				self.frame_idx += 1;
				// self.frame_idx == MAX_FRAME_SIZE is ok here, if this fills the frame to 100.0%
				debug_assert!(self.frame_idx <= MAX_FRAME_SIZE);
			}
		}
	}
}
