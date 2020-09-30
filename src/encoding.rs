//! Handles encoding of a Bitterlemon data stream.
// I/O variant, 0.3.x

use crate::{
	Run,
	Bit,
	run_buffer::RunBuffer,
	MAX_RUN_SIZE,
	MAX_FRAME_SIZE,
};

use std::{
	mem::replace,
};

#[derive(Debug, Default)]
pub struct Encoder {
	run_builder: RunBuilder,
	frame_builder: FrameBuilder,
	run_holding: RunBuffer,
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
		if let Some(next_run) = self.run_builder.update(bit) {
			self.run_holding.push_back(next_run)
				.expect("Encoder::update/1: RunBuffer overflow");
		}

		// try continuining to flush an existing frame
		if let run_byte @ Some(_) = self.frame_builder.try_reduce_run() {
			return run_byte;
		}

		// if there's no run to pass through the frame builder, return immediately
		// otherwise, it'll be taken as a sign to immediately flush the run
		let mut holding = Some(self.run_holding.pop_front()?);
		let r = self.frame_builder.update(&mut holding);
		if let Some(not_consumed) = holding {
			self.run_holding.push_front(not_consumed) // put it back for next time
			.expect("Encoder::update/2: RunBuffer overflow");
			debug_assert!(r.is_some()); // if frame builder didn't consume the bit,
			// it had better be outputting a frame
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
			run_holding.push_back(final_run)
			.expect("Encoder::flush/1: RunBuffer overflow");
		}

		Flush { frame_builder, run_holding }
	}
}

#[derive(Debug)]
pub struct Flush {
	frame_builder: FrameBuilder,
	run_holding: RunBuffer,
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
					self.run_holding.push_front(not_consumed)
					.expect("Flush::next/1: RunBuffer overflow");
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
				let next = output_iter.next();
				assert_eq!(next, Some(output),
					"{:02x} vs {:02x}", next.unwrap_or(0), output);
			}
		}

		let mut flush = encoder.flush();

		for got in &mut flush {
			let next = output_iter.next();
			assert_eq!(next, Some(got),
				"{:02x} vs {:02x}", next.unwrap_or(0), got);
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

	#[test]
	fn run_holding_limit() {
		// Push run holding size to its theoretical limit
		fn make_iter(start: bool) -> impl Iterator<Item = bool> {
			std::iter::successors(Some(start), |last| Some(!*last))
			.take((MAX_FRAME_SIZE as usize)*2)
		}

		case(make_iter(false), &[
			0x00,
			0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa,
			0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa,
			0x00,
			0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa,
			0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa, 0xaa,
		]);

		case(make_iter(true), &[
			0x00,
			0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55,
			0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55,
			0x00,
			0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55,
			0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55, 0x55,
		]);
	}
}


// @@@ IterableEncoder

#[derive(Debug)]
pub struct IterableEncoder<S> {
	inner: EncoderSwitch,
	source: S,
}

pub fn encode<S: Iterator<Item = bool>>(source: S) -> IterableEncoder<S> {
	IterableEncoder {
		inner: EncoderSwitch::Encoder(Encoder::new()),
		source,
	}
}

impl<S: Iterator<Item = bool>> Iterator for IterableEncoder<S> {
	type Item = u8;

	fn next(&mut self) -> Option<Self::Item> {
		let encoder = match self.inner {
			EncoderSwitch::Encoder(ref mut e) => e,
			EncoderSwitch::Flush(ref mut f) => return f.next()
		};

		loop {
			let src_next = match self.source.next() {
				Some(i) => i,
				None => break,
			};

			if let got @ Some(_) = encoder.update(src_next) {
				return got;
			}
		}

		// no more inputs; we may have more outputs for a flushed encoder
		// but if here, we have to perform the switch
		let flush = unsafe {
			use std::{ptr,hint};
			let moved_encoder = ptr::read(encoder as *mut Encoder);
			ptr::write(&mut self.inner as *mut _,
				EncoderSwitch::Flush(moved_encoder.flush())
			);
			// now re-borrow from enum
			match self.inner {
				EncoderSwitch::Flush(ref mut f) => f,
				EncoderSwitch::Encoder(_) => hint::unreachable_unchecked(),
			}
		};

		flush.next()
	}
}

#[derive(Debug)]
enum EncoderSwitch {
	Encoder(Encoder),
	Flush(Flush),
}

#[cfg(test)]
mod test_iterable {
	use super::*;
	use std::iter;

	#[test]
	fn with_runs() {
		let src = iter::repeat(false).take(15)
			.chain(iter::repeat(true).take(15));

		assert_eq!(&[0x8f, 0xcf], &*encode(src).collect::<Vec<_>>());
	}

	#[test]
	fn with_frames() {
		let src = iter::successors(Some(false), |o| Some(!*o))
			.take(20);

		assert_eq!(&[0x14, 0xaa, 0xaa, 0x0a], &*encode(src).collect::<Vec<_>>());

	}
}


// @@@ internals

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
					padding + 8 >= len
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

		if let byte @ Some(_) = self.try_reduce_run() {
			return byte;
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

	fn try_reduce_run(&mut self) -> Option<u8> {
		if let StageFlow::Flush { ref mut flush_idx, stage_size } = self.stage_flow {
			// flush byte of frame, leave 0 behind
			let r = replace(&mut self.frame_stage[*flush_idx as usize], 0);
			*flush_idx += 1;
			if *flush_idx >= stage_size {
				// frame completely sent; reset
				self.reset_stage();
			}
			return Some(r);
		}
		None
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