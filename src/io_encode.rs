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

#[cfg(test)]
mod test_with_frames {
	use super::*;

	fn case(input: &[Run], expected: &[u8]) {
		let mut output = Vec::with_capacity(expected.len());
		let mut encoder = FrameBuilder::new();
		let mut src_iter = input.iter().copied();
		while let mut next @ Some(_) = src_iter.next() {
			loop {
				if let Some(got) = encoder.update(&mut next) {
					// got output byte
					output.push(got);
				}
				if next.is_none() { break; }
			}
		}

		assert_eq!(expected, &*output);
	}

	#[test]
	fn empty_frame() {
		case (&[], &[]);
	}

	#[test]
	fn one_byte_frame_filled() {
		let mut v = Vec::with_capacity(8);
		for _ in 0..4 {
			v.push(Run::Set(1));
			v.push(Run::Clear(1));
		}

		case(v.as_slice(), &[0x08, 0x55]);
	}

	#[test]
	fn one_run() {
		case(&[Run::Set(1)], &[0xc1]);
	}

	#[test]
	fn two_runs() {
		case(&[Run::Clear(1), Run::Set(1)], &[0x02, 0x02]);
	}

	#[test]
	fn byte_nearly_filled() {
		let mut src = Vec::with_capacity(7);
		src.push(Run::Clear(1));
		for _ in 0..3 {
			src.push(Run::Set(1));
			src.push(Run::Clear(1));
		}
		case(src.as_slice(), &[0x07, 0x2a]);
	}

	#[test]
	fn two_bytes() {
		case(
			&[Run::Set(6), Run::Clear(4), Run::Set(6)],
			&[0x10, 0x3f, 0xfc]
		);
	}

	#[test]
	fn never_create_frame_for_longer_runs() {
		case(
			&[Run::Set(16)],
			&[0xd0]
		);
		case(
			&[Run::Clear(16)],
			&[0x90]
		);
		case(
			&[Run::Clear(16), Run::Set(16)],
			&[0x90, 0xd0]
		);
	}

	#[test]
	fn abandon_frame_on_long_runs() {
		case(
			&[Run::Clear(1), Run::Set(2), Run::Clear(16)],
			&[0x03, 0x06, 0x90]
		);
		case(
			&[Run::Clear(20), Run::Set(1), Run::Clear(1), Run::Set(20)],
			&[0x94, 0x02, 0x01, 0xd4]
		);
		case(
			&[Run::Set(1), Run::Clear(1), Run::Set(1), Run::Clear(1), Run::Set(64), Run::Set(4)],
			&[0x04, 0x05, 0xc0, 0xc4]
		);
	}

	#[test]
	fn conditional_on_padding() {
		case(
			&[Run::Set(1), Run::Clear(15)],
			&[0x10, 0x01, 0x00]
		);
		case(
			&[Run::Set(7), Run::Clear(1), Run::Set(8)],
			&[0x10, 0x7f, 0xff]
		);
	}

	#[test]
	fn undo_single_byte_frame() {
		case(
			&[Run::Set(1), Run::Clear(64)],
			&[0xc1, 0x80]
		);
	}

	#[test]
	fn avoid_frame_overflow() {
		// capacities should reflect final compiled size
		let mut inputs = Vec::with_capacity((MAX_FRAME_SIZE as usize) + 1);
		let mut outputs = Vec::with_capacity((MAX_FRAME_SIZE as usize) / 8 + 2);

		// create a set1-clear1 pattern that fills up a frame 100%...
		for _ in 0..(MAX_FRAME_SIZE / 2) {
			inputs.push(Run::Set(1));
			inputs.push(Run::Clear(1));
		}
		// ...then add one more
		inputs.push(Run::Set(1));

		outputs.push(0u8); // frame size
		for _ in 0..(MAX_FRAME_SIZE / 8) {
			outputs.push(0xaa);
		}
		outputs.push(0xc1);

		case(inputs.as_slice(), outputs.as_slice());
	}
}