//! Handles encoding of a Bitterlemon data stream.

use arrayvec;

use std::iter::FusedIterator;

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

impl<S: Iterator<Item = bool>> FusedIterator for Encoder<S> { }

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
}

#[cfg(test)]
mod test_runs {

	use super::Run::*;

	#[test]
	fn len() {
		assert_eq!(12, Set(12).len());
		assert_eq!(23, Set(23).len());
		assert_eq!(34, Clear(34).len());
		assert_eq!(45, Clear(45).len());
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

impl From<Run> for u8 {
	fn from(src: Run) -> u8 {

		let high_bits: u8 = match src {
			Run::Set  (_) => 0xc0,
			Run::Clear(_) => 0x80,
		};

		let run_size_encoded: u8 = match src.len() {
			RLE_MAX_RUN => 0, // encode max run len as 0
			n if n > RLE_MAX_RUN => unreachable!("run too long: {:?}", src),
			other => other, // all valid non-0 values encode as themselves
		};

		high_bits | run_size_encoded
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
		use std::mem::replace;

		fn inc(run: &mut Run) {
			let len = run.len_mut();
			*len = len.wrapping_add(1);
			debug_assert!(*len <= RLE_MAX_RUN);
		}

		loop {
			// Grab next bit from source; early return if None upstream
			let pp = match self.source.next() {
				Some(b) => b,
				None => return replace(&mut self.state, None)
			};

			match self.state {
				// Can't fit any more in this run
				Some(r) if r.len() == RLE_MAX_RUN => {
					self.state = Some(rle_new_run(pp));
					return Some(r);
				},

				// Increase existing Set run
				Some(ref mut run @ Set(_)) if pp
					=> inc(run),

				// Increase existing Clear run
				Some(ref mut run @ Clear(_)) if !pp
					=> inc(run),

				// Swap from Set to Clear
				Some(Set(_)) if ! pp
					=> return replace(&mut self.state, Some(Clear(1))),

				// Swap from Clear to Set
				Some(Clear(_)) if pp
					=> return replace(&mut self.state, Some(Set(1))),

				// All cases covered, but can't convince rustc
				Some(_) => unreachable!("missing Some(Run) case"),

				None => self.state = Some(rle_new_run(pp)),
			}
		}
	}
}

impl<S: Iterator<Item = bool>> FusedIterator for RunIterator<S> { }

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
	fn fused_check() {
		let mut iter = RunIterator::from_bits((&[] as &[bool]).iter().map(|&b| b));
		for i in 0..20 {
			let next = iter.next();
			assert_eq!(None, next, "pulling next() on run {} was {:?}, not None", i, next);
		}
	}
}


type RunHolding = arrayvec::ArrayVec<[Run; RLE_MAX_FRAME as usize]>;

trait RunHoldingExtensions {
	fn bit_count(&self) -> u16;
	fn padding(&self) -> u8;
	fn num_bits(&self) -> u8;
	fn unshift_bit(&mut self, ptr: &mut u8) -> Option<bool>;
	fn try_pop_single(&mut self) -> Option<Run>;
}

impl RunHoldingExtensions for RunHolding {
	fn bit_count(&self) -> u16 {
		self.iter().map(|r| r.len() as u16).sum::<u16>()
	}

	fn padding(&self) -> u8 {
		match self.bit_count() as u8 & 7 {
			0 => 0,
			n => 8 - n,
		}
	}

	fn num_bits(&self) -> u8 {
		let r = self.iter().map(|r| r.len() as u16).sum::<u16>();
		debug_assert!(r <= RLE_MAX_FRAME as u16, "number of frame bits too high at {:?}", r);
		r as u8
	}

	fn unshift_bit(&mut self, ptr: &mut u8) -> Option<bool> {
		let head_run = &mut self.get_mut(*ptr as usize)?;
		let r = matches!(head_run, Run::Set(_));

		let head_size = head_run.len_mut();
		*head_size -= 1;
		if *head_size == 0 {
			*ptr += 1;
		}

		Some(r)
	}

	fn try_pop_single(&mut self) -> Option<Run> {
		if self.len() == 1 {
			match self.pop() {
				x @ Some(_) => x,
				None => unsafe {
					// arrayvec len is 1, so this is guaranteed to never occur
					std::hint::unreachable_unchecked()
				}
			}
		} else { None }
	}
}

#[cfg(test)]
mod run_holding_extensions {

	use super::*;
	use super::Run::*;

	fn op(input: &[Run], frame_size: u8, run_size: u16) {
		let mut builder = RunHolding::new();

		for &element in input {
			builder.push(element);
		}

		let actual_frame_size = (builder.bit_count() // bit count
			+ 7) // round up
			/ 8 // bits to bytes
			+ 1 // add header
			;

		assert_eq!(frame_size as u16, actual_frame_size);
		assert_eq!(run_size, builder.len() as u16);
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

impl<S: Iterator<Item = Run>> WithFrames<S> {
	fn new(source: S) -> WithFrames<S> {
		WithFrames {
			runs: RunHolding::new(),
			mode: WithFramesMode::Filling,
			source,
			next_run: None,
		}
	}

	fn next_should_expand_frame(&mut self, next_run: Run) -> bool {
		// never expand a frame we're flushing
		if matches!(self.mode, WithFramesMode::FlushingFrame(_, _)) {
			return false;
		}

		let run_size = next_run.len();

		// would the frame get too large?
		let cur_frame_size = self.runs.num_bits() as u16;
		if cur_frame_size + (run_size as u16) > (RLE_MAX_FRAME as u16) {
			return false;
		}

		// a frame -> run switch adds between 8 and 15 bits
		// (<= 7 bits frame padding, 8 bits for run)
		if run_size < 8 {
			// never more efficient to close frame
			return true;
		}
		if run_size >= 16 {
			// a frame can't keep up with that
			return false;
		}

		// there are two options:
		// - pad the frame and output a run (cost: padding + 8)
		// - add to the frame (cost: run size)
		// when equal, add to frame -- mitigate pathological cases where consistently
		// opened after balanced runs close them
		self.runs.padding() + 8 >= run_size
	}

	fn next_continue_purge(&mut self) -> Option<<Self as Iterator>::Item> {
		let (to_return, next_mode) : (Option<u8>, Option<WithFramesMode>) = match self.mode {
			WithFramesMode::Filling => {
				if let Some(single) = self.runs.try_pop_single() {
					// special case: told to abandon a one-run frame
					// should just output the run instead
					(Some(single.into()), None /* keep Filling */)
				}
				else if ! self.runs.is_empty() {
					// return header for new frame to output
					// and prime next mode to be WithFramesMode::FlushingFrame
					let frame_size = self.runs.len() as u8;
					let header_byte = self.runs.num_bits();
					(
						Some(if header_byte == RLE_MAX_FRAME {0u8} else {header_byte}),
						Some(WithFramesMode::FlushingFrame(0, frame_size))
					)
				}
				else if let Some(moved_run) = self.next_run {
					// expecting to fill a frame, but was told not to do it here
					// move the run out instead
					self.next_run = None;
					(Some(moved_run.into()), None)
				}
				else {
					// if here, must have previously purged the frame that finishes all iteration
					(None, None)
				}
			},
			WithFramesMode::FlushingFrame(ref mut ptr, ref size) => {
				dbg!((*ptr, *size));
				// mid-frame

				// drain run to fill a byte
				let mut byte = 0u8;
				let mut mask = 7;
				for _ in 0..8 {
					match self.runs.unshift_bit(ptr) {
						Some(bit) => byte |= (bit as u8) << mask,
						None => break,
					}
					mask -= 1;
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

impl<S: Iterator<Item = Run>> Iterator for WithFrames<S> {
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
			let next_run = {
				if self.next_run.is_none() {
					self.next_run = self.source.next();
				}

				match self.next_run {
					Some(r) => r,
					None => break,
				}
			};

			if self.next_should_expand_frame(next_run) {
				self.runs.push(next_run);
				self.next_run = None;
				continue;
			}
			break
		}
		self.next_continue_purge()
	}
}

impl<S: Iterator<Item = Run>> FusedIterator for WithFrames<S> { }

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
	fn fused_check() {
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
