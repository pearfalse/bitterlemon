//! Handles encoding of a Bitterlemon image

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

