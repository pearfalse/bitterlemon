//! Handles encoding of a Bitterlemon data stream.

use arrayvec;

/// Encodes a given bit stream into a compact byte representation.
/// `source` can be any iterator that yields `bool` values.
pub fn encode<S: Iterator<Item = bool>>(source: S) -> Encoder<S> {
	let runs_only = RunIterator::from_bits(source);
	let with_frames = WithFrames::new(runs_only);
	Encoder {
		encoder_impl: with_frames,
	}
}

/// The state for the encode process. This is an iterator that yields encoded bytes as `u8`.
///
/// To perform an encoding, see [`encode`](fn.encode.html).
// Implemented as a naïve run generator, followed by a frame re-pack scanner
pub struct Encoder<S> {
	encoder_impl: WithFrames<RunIterator<S>>,
}

impl<S: Iterator<Item = bool>> Iterator for Encoder<S> {
	type Item = u8;

	fn next(&mut self) -> Option<Self::Item> {
		self.encoder_impl.next()
	}
}

/// Represents a single run.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Run {
	/// Run is of `1`-valued bits
	Set(u8),
	/// Run is of `0`-valued bits
	Clear(u8)
}

impl Run {
	fn len(&self) -> u8 {
		match *self {
			Run::Set(x) => x,
			Run::Clear(x) => x,
		}
	}

	fn len_mut(&mut self) -> &mut u8 {
		match *self {
			Run::Set(ref mut x) => x,
			Run::Clear(ref mut x) => x,
		}
	}

	fn bit(&self) -> u8 {
		match *self {
			Run::Set(_) => 1,
			Run::Clear(_) => 0,
		}
	}
}

#[cfg(test)]
mod test_runs {

	use super::Run::*;

	#[test]
	fn len() {
		assert_eq!(50, Set(50).len());
		assert_eq!(50, Clear(50).len());
		assert_eq!(Set(23).len(), Clear(23).len());
	}

	#[test]
	fn len_mut() {
		macro_rules! implement {
			($e:ident) => (
				let mut run = super::Run::$e(20);
				assert_eq!(20, *run.len_mut());

				{
					let r = run.len_mut();
					*r += 2;
				}
				assert_eq!($e(22), run);
			)
		}

		implement!(Set);
		implement!(Clear);
	}
}

impl Into<u8> for Run {
	fn into(self) -> u8 {

		use self::Run::*;

		match self {
			Set(n)   => {
				assert!(n <= RLE_MAX_RUN);
				let n_out = if n == RLE_MAX_RUN {0u8} else {n};
				0xc0u8 + n_out
			},
			Clear(n) => {
				assert!(n <= RLE_MAX_RUN);
				let n_out = if n == RLE_MAX_RUN {0u8} else {n};
				0x80u8 + n_out
			},
		}

	}
}

#[derive(Debug)]
struct RunIterator<S> {
	state: Option<Run>,
	source: S,
}

fn rle_new_run(pp: bool) -> Run {
	if pp {
		Run::Set(1)
	} else {
		Run::Clear(1)
	}
}

const RLE_MAX_RUN: u8 = 64;
const RLE_MAX_FRAME: u8 = 128;

impl<S: Iterator<Item = bool>> RunIterator<S> {
	fn from_bits(source: S) -> RunIterator<S> {
		RunIterator {
			state: None,
			source,
		}
	}
}

impl<S: Iterator<Item=bool>> Iterator for RunIterator<S> {
	type Item = Run;

	fn next(&mut self) -> Option<Run> {
		use self::Run::*;
		loop {

			// Grab next bit from source; early return if None upstream
			let pp = match self.source.next() {
				Some(b) => b,
				None    => return match self.state {
					Some(x) => {
						// Push out current state and clear for next time
						let old = x;
						self.state = None;
						Some(old)
					},
					// All done
					None => None
				}
			};

			match self.state {

				// Can't fit any more in this run
				Some(Set(RLE_MAX_RUN)) => {
					self.state = Some(rle_new_run(pp));
					return Some(Set(RLE_MAX_RUN));
				},

				// Can't fit any more in this run
				Some(Clear(RLE_MAX_RUN)) => {
					self.state = Some(rle_new_run(pp));
					return Some(Clear(RLE_MAX_RUN));
				}

				// Increase existing Set run
				Some(Set(count)) if pp => {
					self.state = Some(Set(count + 1));
					continue;
				},

				// Swap from Set to Clear
				Some(Set(_)) if ! pp => {
					let old = self.state;
					self.state = Some(Clear(1));
					return old;
				},

				// Increase existing Clear run
				Some(Clear(count)) if ! pp => {
					self.state = Some(Clear(count + 1));
					continue;
				},

				// Swap from Clear to Set
				Some(Clear(_)) if pp => {
					let old = self.state;
					self.state = Some(Set(1));
					return old;
				},

				// All cases covered, but can't convince rustc
				Some(_) => {
					unreachable!();
				}

				None => {
					self.state = Some(rle_new_run(pp));
					continue;
				}
			}

		}

	}
}

#[cfg(test)]
mod test_run_builder {

	use std::iter::Iterator;

	use super::{Run, RunIterator};
	use super::Run::*;

	#[test]
	fn runs() {
		let op = |input: &[u8], output: &[Run]| {

			let bool_stream : Vec<bool> = input.iter().map(|&c| c == b'1').collect();
			let collected : Vec<Run> = RunIterator::from_bits(bool_stream.iter().map(|&b| b)).collect();

			assert_eq!(output, collected.as_slice());
		};

		// simple single runs
		op(b"1", &[Set(1)]);
		op(b"1111", &[Set(4)]);
		op(b"00", &[Clear(2)]);
		op(b"", &[]);

		// alterations
		op(b"011", &[Clear(1), Set(2)]);
		op(b"110", &[Set(2), Clear(1)]);
		op(b"0101", &[Clear(1), Set(1), Clear(1), Set(1)]);

		// run size limits
		let large_set = [b'1'; (super::RLE_MAX_RUN + 1) as usize];
		op(&large_set, &[Set(super::RLE_MAX_RUN), Set(1)]);

		// clear size limits
		let large_clear = [b'0' ; (super::RLE_MAX_RUN + 1) as usize];
		op(&large_clear, &[Clear(super::RLE_MAX_RUN), Clear(1)]);
	}

	#[test]
	fn next_none_idempotence() {
		let mut iter = RunIterator::from_bits((&[] as &[bool]).iter().map(|&b| b));
		for i in 0..20 {
			let next = iter.next();
			assert_eq!(None, next, "pulling next() on run {} was {:?}, not None", i, next);
		}
	}
}


type RunHolding = arrayvec::ArrayVec<[Run; 128]>;

trait RunHoldingExtensions {
	fn bytes_as_frame(&self) -> (u8, u8);
	fn num_bits(&self) -> u8;
	fn unshift_bit(&mut self, ptr: &mut u8) -> u8;
}

impl RunHoldingExtensions for RunHolding {
	fn bytes_as_frame(&self) -> (u8, u8) {
		let total_bits: usize = self.iter().map(|&run| match run {
			Run::Set(x) => x as usize,
			Run::Clear(x) => x as usize,
		}).sum();

		let total_bits = total_bits + 8; // for the header
		let bytes = ((total_bits + 7) >> 3) as u8;
		let padding = (total_bits & 7) as u8;
		(bytes, padding)
	}

	fn num_bits(&self) -> u8 {
		let r : usize = self.iter().map(|r| r.len() as usize).sum();
		debug_assert!(r <= RLE_MAX_FRAME as usize, "number of frame bits too high at {:?}", r);
		r as u8
	}

	fn unshift_bit(&mut self, ptr: &mut u8) -> u8 {
		let head_run = self.get_mut(*ptr as usize).unwrap();
		let r = head_run.bit();

		let head_size = head_run.len_mut();
		*head_size -= 1;
		if *head_size == 0 {
			*ptr += 1;
		}

		r
	}
}

#[cfg(test)]
mod run_holding_extensions {

	use super::*;
	use super::Run::*;

	fn op(input: &[Run], frame_size: u8, run_size: u16) {
		let mut builder = RunHolding::new();

		for element in input {
			builder.push(element.clone());
		}

		assert_eq!(frame_size, builder.bytes_as_frame().0);
		assert_eq!(run_size, builder.len() as u16);
	}

	#[test]
	fn empty() {
		op(&[], 1, 0);
	}

	#[test]
	fn single_set() {
		op(&[Set(1)], 2, 1);
	}

	#[test]
	fn single_clear() {
		op(&[Clear(1)], 2, 1);
	}
	#[test]
	fn enough_to_overflow_a_frame_to_2_post_header_bytes() {
		op(&[Set(5), Clear(5)], 3, 2);
	}

	#[test]
	fn can_pack_3_runs_into_a_single_frame_byte() {
		op(&[Clear(3), Set(2), Clear(3)], 2, 3);
	}
}


#[derive(Debug, PartialEq, Eq)]
struct WithFrames<S> {
	runs: RunHolding,
	mode: WithFramesMode,
	source: S,
	next_run: Option<Run>,
}

#[derive(Debug, PartialEq, Eq)]
enum WithFramesMode {
	Filling,
	FlushingFrame(u8, u8), // ptr, length
}

impl<S: Iterator<Item=Run>> WithFrames<S> {
	fn new(source: S) -> WithFrames<S> {
		WithFrames {
			runs: RunHolding::new(),
			mode: WithFramesMode::Filling,
			source,
			next_run: None,
		}
	}

	fn next_should_expand_frame(&mut self) -> bool {
		// never expand a frame we're flushing
		if matches!(self.mode, WithFramesMode::FlushingFrame(_, _)) {
			return false;
		}

		let run_size = self.next_run.unwrap().len();

		// would the frame get too large?
		let cur_frame_size = self.runs.num_bits() as u16;
		if cur_frame_size + (run_size as u16) > (RLE_MAX_FRAME as u16) { return false; }

		// let's get some clear cases out of the way first
		if run_size < 8 { return true; }
		if run_size >= 16 { return false; }


		let (_, padding) = self.runs.bytes_as_frame();
		// only add to the frame if the frame size increase is < 2 bytes
		// IOW, run size - frame padding < 16 bits
		run_size - padding < 16
	}

	fn next_add_to_frame(&mut self)  {
		let next_run = self.next_run.take().unwrap();
		self.runs.push(next_run);
	}

	fn next_continue_purge(&mut self) -> Option<<Self as Iterator>::Item> {
		let (to_return, next_mode) : (Option<u8>, Option<WithFramesMode>) = match self.mode {
			WithFramesMode::Filling => {
				if self.runs.len() == 1 {
					// special case: told to abandon a one-run frame
					// should just output the run instead
					let moved_run = self.runs.pop().unwrap();
					(Some(moved_run.into()), None /* keep Filling */)
				}
				else if self.runs.len() > 0 {
					// return header for new frame to output
					// and prime next mode to be WithFramesMode::FlushingFrame
					let frame_size = self.runs.len() as u8;
					let header_byte = self.runs.num_bits();
					(
						Some(if header_byte == RLE_MAX_FRAME {0u8} else {header_byte}),
						Some(WithFramesMode::FlushingFrame(0, frame_size))
					)
				}
				else if self.next_run.is_some() {
					// expecting to fill a frame, but was told not to do it here
					// move the run out instead
					let moved_run = self.next_run.take().unwrap();
					(Some(moved_run.into()), None)
				}
				else {
					// if here, must have previously purged the frame that finishes all iteration
					(None, None)
				}
			},
			WithFramesMode::FlushingFrame(ref mut ptr, ref size) => {
				// mid-frame

				// drain run to fill a byte
				let mut byte = 0u8;
				let mut mask = 7;
				for _ in 0..8 {

					byte |= self.runs.unshift_bit(ptr) << mask;
					mask -= 1;

					if ptr == size {
						// run exhausted
						break;
					}
				}

				let next_mode = if ptr == size {
					self.runs = RunHolding::new(); // reset run holder
					Some(WithFramesMode::Filling)
				} else { None };
				(Some(byte), next_mode)
			},
		};

		if let Some(next_mode) = next_mode {
			self.mode = next_mode;
		}

		to_return
	}
}

impl<S: Iterator<Item=Run>> Iterator for WithFrames<S> {
	type Item = u8;

	fn next(&mut self) -> Option<Self::Item> {
		// check next run size.
		// if size < 8:
		//   add run to current frame
		// if 8 <= size < 16:
		//   let increase = see how much the frame would increase by, in bytes if you added this run
		//   if increase > 1:
		//     flush current frame, allowing next run to part-pour into frame padding
		//   else:
		//     add run to current frame
		// else:
		//   flush current frame

		loop {
			// get next element, if there is one
			if self.next_run.is_none() {
				self.next_run = self.source.next();
			}

			if self.next_run.is_none() {
				return self.next_continue_purge();
			}

			if self.next_should_expand_frame() {
				self.next_add_to_frame();
				continue;
			}

			return self.next_continue_purge();
		}
	}
}

#[cfg(test)]
mod test_with_frames {
	use super::*;

	macro_rules! from_these {
		($slice:expr) => {
			WithFrames::new(($slice as &[Run]).into_iter().cloned())
		}
	}

	fn case(input: &[Run], expected: &[u8]) {
		let output : Vec<u8> = from_these!(input).collect();
		assert_eq!(expected, output.as_slice());
	}

	#[test]
	fn empty_frame() {
		case (&[], &[]);
	}

	#[test]
	fn next_none_idempotence() {
		let mut iter = WithFrames::new((&[] as &[Run]).iter().map(|&b| b));
		for i in 0..20 {
			let next = iter.next();
			assert_eq!(None, next, "pulling next() on run {} was {:?}, not None", i, next);
		}
	}

	#[test]
	fn one_byte_frame_filled() {
		let mut v = Vec::with_capacity(8);
		for _ in 0..4 {
			v.push(Run::Set(1));
			v.push(Run::Clear(1));
		}

		case(v.as_slice(), &[0x08, 0xaa]);
	}

	#[test]
	fn one_run() {
		case(&[Run::Set(1)], &[0xc1]);
	}

	#[test]
	fn two_runs() {
		case(&[Run::Clear(1), Run::Set(1)], &[0x02, 0x40]);
	}

	#[test]
	fn byte_nearly_filled() {
		let mut src = Vec::with_capacity(7);
		src.push(Run::Clear(1));
		for _ in 0..3 {
			src.push(Run::Set(1));
			src.push(Run::Clear(1));
		}
		case(src.as_slice(), &[0x07, 0x54]);
	}

	#[test]
	fn two_bytes() {
		case(&[Run::Set(6), Run::Clear(4), Run::Set(6)], &[0x10, 0xfc, 0x3f]);
	}

	#[test]
	fn never_create_frame_for_longer_runs() {
		let execute = |inputs: &[Run]| {
			let output : Vec<u8> = from_these!(inputs).collect();
			let direct_bytes : Vec<u8> = inputs.iter().map(|r| (*r).into()).collect();
			assert_eq!(direct_bytes, output);
		};

		execute(&[Run::Set(16)]);
		execute(&[Run::Clear(16)]);
		execute(&[Run::Clear(16), Run::Set(16)]);
	}

	#[test]
	fn abandon_frame_on_long_runs() {

		case(&[Run::Clear(1), Run::Set(2), Run::Clear(16)], &[0x03, 0x60, 0x90]);
		case(&[Run::Clear(20), Run::Set(1), Run::Clear(1), Run::Set(20)], &[0x94, 0x02, 0x80, 0xd4]);
		case(&[Run::Set(1), Run::Clear(1), Run::Set(1), Run::Clear(1), Run::Set(64), Run::Set(4)], &[0x04, 0xa0, 0xc0, 0xc4]);
	}

	#[test]
	fn conditional_on_padding() {
		case(&[Run::Set(1), Run::Clear(15)], &[0x10, 0x80, 0x00]);
		case(&[Run::Set(7), Run::Clear(1), Run::Set(8)], &[0x10, 0xfe, 0xff]);
	}

	#[test]
	fn undo_single_byte_frame() {
		case(&[Run::Set(1), Run::Clear(64)], &[0xc1, 0x80]);
	}

	#[test]
	fn avoid_frame_overflow() {
		// capacities should reflect final compiled size
		let mut inputs = Vec::with_capacity((RLE_MAX_FRAME as usize) + 1);
		let mut outputs = Vec::with_capacity((RLE_MAX_FRAME as usize) / 8 + 2);

		// create a set1-clear1 pattern that fills up a frame 100%...
		for _ in 0..(RLE_MAX_FRAME / 2) {
			inputs.push(Run::Set(1));
			inputs.push(Run::Clear(1));
		}
		// ...then add one more
		inputs.push(Run::Set(1));

		outputs.push(0u8); // frame size
		for _ in 0..(RLE_MAX_FRAME / 8) {
			outputs.push(0xaa);
		}
		outputs.push(0xc1);

		case(inputs.as_slice(), outputs.as_slice());
	}
}
