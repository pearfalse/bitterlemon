//! Handles encoding of a Bitterlemon image

extern crate arrayvec;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Run {
	Set(u8),
	Clear(u8)
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

type RunHolding = arrayvec::ArrayVec<[Run; 128]>;

#[derive(Debug, PartialEq, Eq)]
pub struct WithFrames<S> {
	runs: RunHolding,
	source: S
}

impl<S: Iterator<Item=Run>> WithFrames<S> {
	pub fn new(source: S) -> WithFrames<S> {
		WithFrames {
			runs: RunHolding::new(),
			source: source,
		}
	}
}

fn bytes_as_frame(runs: &RunHolding) -> usize {
	let total_bits: usize = runs.iter().map(|&run| match run {
		Run::Set(x) => x as usize,
		Run::Clear(x) => x as usize,
	}).sum();

	(total_bits + 15) >> 3 // round up to next byte, add header
}

fn bytes_as_runs(runs: &RunHolding) -> usize {
	// runs in bytes are next highest multiples of 64
	runs.iter().map(|&run| {
		let found = match run {
			Run::Set(x) => x,
			Run::Clear(x) => x,
		};
		debug_assert!(found > 0);
		(found + 63) >> 6
	} as usize).sum()
}


#[cfg(test)]
mod test_frame_sizer {

	use super::*;
	use super::Run::*;

	fn op(input: &[Run], frame_size: usize, run_size: usize) {
		let mut builder = RunHolding::new();

		for element in input {
			assert_eq!(None, builder.push(element.clone()));
		}

		assert_eq!(frame_size, bytes_as_frame(&builder));
		assert_eq!(run_size, bytes_as_runs(&builder));
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

impl<S> Iterator for WithFrames<S> {
	type Item = u8;

	fn next(&mut self) -> Option<Self::Item> {
		unimplemented!();
	}

}


#[cfg(test)]
mod test_runs {

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

