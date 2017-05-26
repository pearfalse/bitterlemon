//! Handles encoding of a Bitterlemon image

extern crate arrayvec;

use std::iter;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Run {
	Set(u8),
	Clear(u8)
}

impl Run {
	fn size(&self) -> u8 { // TODO rename this to len for Rusticity
		match *self {
			Run::Set(x) => x,
			Run::Clear(x) => x,
		}
	}

	fn size_mut<'a>(&'a mut self) -> &'a mut u8 {
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
	fn size() {
		assert_eq!(50, Set(50).size());
		assert_eq!(50, Clear(50).size());
		assert_eq!(Set(50).size(), Clear(50).size());
	}

	#[test]
	fn size_mut() {
		macro_rules! implement {
			($e:ident) => (
				let mut run = super::Run::$e(20);
				assert_eq!(20, *run.size_mut());

				{
					let r = run.size_mut();
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
pub struct RunIterator<S> {
	state: Option<Run>,
	source: S,
}

fn rle_new_run(pp: bool) -> Run {
	if pp {Run::Set(1)} else {Run::Clear(1)}
}

const RLE_MAX_RUN: u8 = 64;

impl<S: Iterator<Item=bool>> RunIterator<S> {

	pub fn from_pixels(source: S) -> RunIterator<S> {
		RunIterator {
			state: None,
			source: source
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
			let collected : Vec<Run> = RunIterator::from_pixels(bool_stream.iter().map(|&b| b)).collect();

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
}


type RunHolding = arrayvec::ArrayVec<[Run; 128]>;

trait RunHoldingExtensions {
	fn bytes_as_frame(&self) -> (u8, u8);
	fn bytes_as_runs(&self) -> u16;
	fn num_pixels(&self) -> u8;
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

	fn bytes_as_runs(&self) -> u16 {
		// runs in bytes are next highest multiples of 64
		self.iter().map(|&run| {
			let found = match run {
				Run::Set(x) => x,
				Run::Clear(x) => x,
			};
			debug_assert!(found > 0);
			(found + 63) >> 6
		} as u16).sum()
	}

	fn num_pixels(&self) -> u8 {
		self.iter().map(Run::size).sum()
	}

	fn unshift_bit(&mut self, ptr: &mut u8) -> u8 {
		let mut head_run = self.get_mut(*ptr as usize).unwrap();
		let r = head_run.bit();

		let head_size = head_run.size_mut();
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
			assert_eq!(None, builder.push(element.clone()));
		}

		assert_eq!(frame_size, builder.bytes_as_frame().0);
		assert_eq!(run_size, builder.bytes_as_runs());
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
pub struct WithFrames<S> {
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
	pub fn new(source: S) -> WithFrames<S> {
		WithFrames {
			runs: RunHolding::new(),
			mode: WithFramesMode::Filling,
			source: source,
			next_run: None,
		}
	}

	fn next_should_expand_frame(&mut self) -> bool {
		let run_size = self.next_run.unwrap().size();

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
		println!("{:?}", self.mode);
		let (to_return, next_mode) : (Option<u8>, Option<WithFramesMode>) = match self.mode {
			WithFramesMode::Filling => {

				if let Some(_) = self.next_run {
					// expecting to fill a frame, but was told not to do it here
					// move the run out instead
					let moved_run = self.next_run.take().unwrap().into();
					(Some(moved_run), None)
				}
				else if self.runs.len() > 0 {
					// return header for new frame to output
					// and prime next mode to be WithFramesMode::FlushingFrame
					let frame_size = self.runs.len() as u8;
					(Some(self.runs.num_pixels()), Some(WithFramesMode::FlushingFrame(0, frame_size)))
				}
				else {
					// if here, must have previously purged the frame that finishes all iteration
					(None, None)
				}
			},
			WithFramesMode::FlushingFrame(ref mut ptr, ref size) => {
				// println!("Flushing frame; holding = {:?}", self.runs);
				// mid-frame

				// drain run to fill a byte
				let mut byte = 0u8;
				let mut mask = 7;
				for i in 0..8 {

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

	#[test]
	fn empty_frame() {
		let mut src = from_these!(&[]);
		assert!(src.next() == None);
	}

	#[test]
	fn one_byte_frame_filled() {
		let mut v = Vec::with_capacity(8);
		for _ in 0..4 {
			v.push(Run::Set(1));
			v.push(Run::Clear(1));
		}

		let src = from_these!(v.as_slice());
		let output : Vec<u8> = src.collect();
		assert_eq!(&[0x08, 0xaa], output.as_slice());
	}

	#[test]
	fn one_bit() {
		let output : Vec<u8> = from_these!(&[Run::Set(1)]).collect();
		assert_eq!(&[0x01, 0x80], output.as_slice());
	}

	#[test]
	fn byte_nearly_filled() {
		let mut src = Vec::with_capacity(7);
		src.push(Run::Clear(1));
		for _ in 0..3 {
			src.push(Run::Set(1));
			src.push(Run::Clear(1));
		}
		let output : Vec<u8> = from_these!(src.as_slice()).collect();
		assert_eq!(&[0x07, 0x54], output.as_slice());
	}

	#[test]
	fn two_bytes() {
		let output : Vec<u8> = from_these!(&[Run::Set(6), Run::Clear(4), Run::Set(6)]).collect();

		assert_eq!(&[0x10, 0xfc, 0x3f], output.as_slice());
	}
}
