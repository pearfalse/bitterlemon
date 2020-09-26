//! Handles encoding of a Bitterlemon data stream.
// I/O variant, 0.3.x

use crate::{
	encode::Run,
	MAX_RUN_SIZE,
	MAX_FRAME_SIZE,
};

use std::{
	collections::VecDeque,
	mem::{replace, transmute},
};

#[derive(Debug, Default)]
pub struct Encoder {
	run_builder: RunBuilder,
	frame_builder: FrameBuilder,
	// TODO: this will have a max capacity it can *ever* grow to, and we can
	// replace the Vec with ArrayVec or somesuch
	run_holding: VecDeque<Run>,
}

impl Encoder {
	pub fn new() -> Encoder {
		Encoder::default()
	}

	/// Updates the encoder state.
	///
	/// It can take multiple bits to yield an encoded byte, so this method may return
	/// `None` for the first few invocations. The encode should be considered complete
	/// when this method returns `None` after being passed `None` as the `bit` argument.
	/// At this point, you can simply drop the Encoder.
	pub fn update(&mut self, bit: bool) -> Option<u8> {
		// when a bit is passed in, but no run comes out, return immediately --
		// do NOT pass None to frame_builder, which will infer that as source EOF
		self.run_holding.push_back(self.run_builder.update(bit)?);

		let mut holding = self.run_holding.pop_front();
		let r = self.frame_builder.update(dbg!(&mut holding));
		if let Some(not_consumed) = holding {
			self.run_holding.push_front(not_consumed); // put it back for next time
		}
		r
	}

	pub fn flush(self) -> Flush {
		let Encoder {
			run_builder,
			frame_builder,
			mut run_holding
		} = self;

		if let Some(final_run) = run_builder.flush() {
			run_holding.push_back(final_run);
		}

		Flush { frame_builder, run_holding }
	}
}

#[derive(Debug)]
pub struct Flush {
	frame_builder: FrameBuilder,
	run_holding: VecDeque<Run>,
}

impl Iterator for Flush {
	type Item = u8;

	fn next(&mut self) -> Option<Self::Item> {

		loop {
			// feed in all cached runs, pausing when something comes out
			let mut next_run = self.run_holding.pop_front();
			if next_run.is_none() {
				break;
			}
			let r = self.frame_builder.update(&mut next_run);
			if r.is_some() {
				// ready to return; ensure we don't lose a run
				if let Some(not_consumed) = next_run {
					self.run_holding.push_front(not_consumed);
				}
				return r;
			}
		}

		// we will now only get a final flushed frame
		self.frame_builder.update(&mut None)
	}
}


#[cfg(test)]
mod test_encoder {
	use super::*;

	fn convert(s: &[u8]) -> impl Iterator<Item = bool> + '_ {
		s.iter().map(|&c| c == b'1')
	}

	fn case(input: impl IntoIterator<Item = bool>, expected: &[u8]) {
		let mut output_iter = expected.iter().copied();
		let mut encoder = Encoder::new();

		for bit in input {
			// new bit from input
			if let Some(output) = encoder.update(bit) {
				assert_eq!(output_iter.next(), Some(output));
			}
		}

		eprintln!("source iterator dried up");
		let mut flush = encoder.flush();

		for got in &mut flush {
			assert_eq!(output_iter.next(), Some(got));
		}

		assert_eq!(None, output_iter.next());
		assert_eq!(None, flush.next());
	}

	#[test]
	fn just_runs() {
		case(convert(b"00000000001111111111"), &[0x8a, 0xca]);
	}

	#[test]
	fn just_frames() {
		case(convert(b"1001001001001010"), &[0x10, 0x49, 0x52]);
	}

	#[test]
	fn mixes() {
		case(convert(b"1100110011111111111101010101"), &[0x08, 0x33, 0xcc, 0x08, 0xaa]);
	}
}


#[derive(Debug, Default)]
struct RunBuilder {
	current: Option<Run>,
}

impl RunBuilder {
	fn new() -> RunBuilder {
		RunBuilder::default()
	}

	fn update(&mut self, bit: bool) -> Option<Run> {
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

	fn flush(self) -> Option<Run> {
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


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
#[allow(dead_code)] // variants are used via `inc` and `dec`, which transmute
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
				transmute((self as u8).wrapping_add(1))
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
				transmute((self as u8).wrapping_sub(1))
			}, false)
		}
	}
}

impl std::ops::Deref for Bit {
	type Target = u8;
	fn deref(&self) -> &Self::Target {
		// u8 is a strict superset of Self, and we don't impl DerefMut
		unsafe { transmute(self) }
	}
}

#[derive(Debug)]
enum StageFlow {
	Fill {
		stage_idx: u8,
		stage_bit: Bit,
	},
	Flush {
		flush_idx: u8,
		stage_size: u8,
	}
}

impl StageFlow {
	/// Migrates a stage flow from fill to flush, returning the header byte.
	fn flush(&mut self) -> u8 {
		match *self {
			StageFlow::Fill { stage_idx, stage_bit } => {
				let raw_stage_size = stage_idx * 8 + *stage_bit;
				eprintln!("Now flushing, raw stage size {}", raw_stage_size);
				*self = StageFlow::Flush {
					flush_idx: 0,
					// copy stage_idx, add 1 if some bits have been written to
					stage_size: stage_idx + u8::from(stage_bit > Bit::Bit0)
				};
				if raw_stage_size == MAX_FRAME_SIZE { 0 } else { raw_stage_size }
			},
			_ => {unreachable!("called `flush` for non-Fill stage flow")}
		}
	}

	fn reset(&mut self) {
		*self = StageFlow::default();
	}
}

impl Default for StageFlow {
	fn default() -> Self {
		StageFlow::Fill {
			stage_idx: 0, stage_bit: Bit::Bit0,
		}
	}
}

#[derive(Debug, Default)]
struct FrameBuilder {
	// assemble frame here
	frame_stage: [u8; STAGE_SIZE],

	// index of frame byte to add to
	stage_flow: StageFlow,

	// flushing edge case
	frame_single_run: Option<Run>,
}

impl FrameBuilder {
	pub fn new() -> FrameBuilder {
		FrameBuilder::default()
	}

	pub fn update(&mut self, run: &mut Option<Run>) -> Option<u8> {
		// The given run will not be `take`n if it's deemed too big for an active frame,
		// which needs to be flushed first

		let mut add_run_to_frame = None;
		if let Some(len) = run.map(|r| r.len()) {
			// we were given a run, and we have its length
			if let StageFlow::Fill { stage_idx, stage_bit } = self.stage_flow {
				if (|| {
					// there's a run we can pull, and a frame we're assembling
					// to add or not to add? the options are:
					// - pad the frame and output a run (cost: padding + 8)
					// - add to the frame (cost: run size)
					// when equal, add to frame -- mitigate pathological cases where
					// frames are consistently opened after balanced runs close them

					// would the frame get too large?
					let try_frame_size = stage_idx * 8 + *stage_bit + len;
					if try_frame_size > MAX_FRAME_SIZE {
						return false;
					}

					if len >= 16 { return false; } // too big to benefit
					if len < 8 { return true; } // always beneficial

					let padding = (8 - *stage_bit) & 7;
					dbg!(padding) + 8 >= len
				})() {
					add_run_to_frame = run.take(); // will be Some()
				}
			}
		}

		if add_run_to_frame.is_none()
		&& !matches!(self.stage_flow, StageFlow::Flush {..}) {
			// not currently flushing frame, but no frame to add

			if self.stage_is_empty() {
				// stage is empty; pass through a run you might have
				return run.take().map(u8::from);
			}

			// before setting up a frame flush: does it just contain a single run?
			// if so, just jump that out as a run
			if let Some(just_one_run) = self.frame_single_run.take() {
				self.reset_stage();
				return Some(just_one_run.into());
			}

			// start flushing this frame; we'll output its header now
			let header = self.stage_flow.flush();
			return Some(header);
		}

		if let StageFlow::Flush { ref mut flush_idx, stage_size } = self.stage_flow {
			// flush byte of frame, leave 0 behind
			let r = replace(&mut self.frame_stage[*flush_idx as usize], 0);
			*flush_idx += 1;
			if *flush_idx >= stage_size {
				// frame completely sent; reset
				eprintln!("Frame sent; resetting");
				self.reset_stage();
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
		// if this is the first run in the frame, copy it
		// in case it ends up being the only frame run
		if self.stage_is_empty() {
			self.frame_single_run = Some(run);
		} else {
			self.frame_single_run = None;
		}

		let (stage_idx, stage_bit) = match self.stage_flow {
			StageFlow::Fill { ref mut stage_idx, ref mut stage_bit }
				=> (stage_idx, stage_bit),
			_
				=> unreachable!("incorrect flow state for pour_run_into_frame"),
		};

		let bit = u8::from(run.bit());
		for _ in 0..run.len() {
			let tgt = &mut self.frame_stage[*stage_idx as usize];
			*tgt |= bit << **stage_bit;

			// increment bit
			let (new_stage_bit, wrap_idx) = stage_bit.inc();
			*stage_bit = new_stage_bit;
			if wrap_idx {
				*stage_idx += 1;
				// self.stage_idx == MAX_FRAME_SIZE is ok here, if this fills the frame to 100.0%
				debug_assert!(*stage_idx <= MAX_FRAME_SIZE);
			}
		}
	}

	fn reset_stage(&mut self) {
		self.stage_flow.reset();
	}

	fn stage_is_empty(&self) -> bool {
		match self.stage_flow {
			StageFlow::Fill { stage_idx, stage_bit }
				=> stage_idx == 0 && stage_bit == Bit::Bit0,
			_ => unreachable!("called `stage_is_empty` for non-Fill stage"),
		}
	}
}

#[cfg(test)]
mod test_with_frames {
	use super::*;
	use hex_slice::AsHex;

	fn case(input: &[Run], expected: &[u8]) {
		println!("\nNew case: {:?} -> {:02x}", input, expected.as_hex());
		let mut output = Vec::with_capacity(expected.len());
		let mut encoder = FrameBuilder::new();
		let mut src_iter = input.iter().copied();
		while let mut next @ Some(_) = src_iter.next() {
			while next.is_some() {
				if let Some(got) = encoder.update(&mut next) {
					// got output byte
					println!("Got stage1 output {:02x}", got);
					output.push(got);
				}
			}
		}

		// no more inputs; catch all staged outputs
		while let Some(flushed) = encoder.update(&mut None) {
			println!("Got stage2 output {:02x}", flushed);
			output.push(flushed);
		}

		assert_eq!(expected, &*output,
			"expected {:02x}, got {:02x}", expected.as_hex(), (&*output).as_hex());
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
		case(&[Run::Set(1)], &[0xc1]);

		case(
			&[Run::Set(1), Run::Clear(64)],
			&[0xc1, 0x80]
		);
	}

	#[test]
	fn clear_stage() {
		// ensure bits set from previous frames are 0'd out when used again
		case(&[
			Run::Set(1), Run::Clear(1),
			Run::Set(1), Run::Clear(1),
			Run::Set(1), Run::Clear(1),
			Run::Set(1), Run::Clear(1),
			Run::Set(1), Run::Clear(1),
			Run::Set(MAX_RUN_SIZE),
			Run::Clear(1), Run::Set(1),
			Run::Clear(1), Run::Set(1),
			Run::Clear(1), Run::Set(1),
			Run::Clear(1), Run::Set(1),
			Run::Clear(1), Run::Set(1),
		], &[
			0x0a, 0x55, 0x01,
			0xc0,
			0x0a, 0xaa, 0x02,
		]);
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
			outputs.push(0x55);
		}
		outputs.push(0xc1);

		case(inputs.as_slice(), outputs.as_slice());
	}
}