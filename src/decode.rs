//! Decodes a Bitterlemon-encoded byte stream into its original bit stream.

use std::iter::Iterator;

pub fn decode<S>(input: S) -> Decoder<S>
where S : Iterator<Item=u8> {
	Decoder::<S> {
		source: input,
		state: DecodeState::Pending,
	}
}

pub struct Decoder<S> {
	state: DecodeState,
	source: S,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
	TruncatedInput {
		pixels_lost: u8,
		bytes_missing: u8,
	},
}

type IterationResult = Result<bool, Error>;

impl<S> Iterator for Decoder<S>
where S : Iterator<Item=u8> {
	type Item = IterationResult;

	fn next(&mut self) -> Option<Self::Item> {

		// pull from source if needs be
		if self.must_pull() {
			let next = self.source.next();
			self.next_with_pulled(next)
		} else {
			self.next_from_existing()
		}
	}
}

impl<S> Decoder<S>
where S : Iterator<Item=u8> {

	fn must_pull(&self) -> bool {
		match self.state {
			DecodeState::Pending => true,
			DecodeState::Done => false,
			DecodeState::Run(remaining, _) => remaining == 0,
			DecodeState::Frame(remaining, _, stage_size) => remaining == 0 || stage_size == 0,
		}
	}

	fn next_with_pulled(&mut self, next: Option<u8>) -> Option<<Self as Iterator>::Item> {

		// handle None from source
		let next = match next {
			Some(x) => x,
			None => match self.state {
				DecodeState::Pending => {
					self.state = DecodeState::Done;
					return None;
				}, // source was empty
				DecodeState::Done    => { return None; }, // always return None here
				DecodeState::Run(_, _) => {
					unreachable!("next_with_pulled called with more run bits to flush: {:?}", self.state);
				},
				DecodeState::Frame(remaining, _, stage_size) => {
					debug_assert!(stage_size == 0);
					debug_assert!(remaining > 0);

					// missing bytes to complete the frame
					let error_specifics = Error::TruncatedInput {
						pixels_lost: remaining,
						bytes_missing: (remaining + 7) >> 3,
					};
					return Some(Err(error_specifics));
				}
			}
		};

		// handle mid-frame
		if match self.state {
			DecodeState::Frame(ref mut remaining, ref mut stage, ref mut stage_size)
			if *remaining > 0 => {
				debug_assert!(*stage_size == 0); // shouldn't have pulled otherwise
				*stage = next;
				*stage_size = 8;
				*remaining += 8;
				// now fall through to real iteration logic
				true
			},
			_ => false
		} {
			return self.next_from_existing();
		}

		let got = match next {
			n if n < 0x80 => {
				// frame beginning
				let frame_size = byte_to_frame_size(n);
				self.state = DecodeState::Frame(frame_size, 0, 0);
				None
			},
			n => {
				// new run
				let frame_size = byte_to_run_size(n);
				let mode = n >= 0xc0;
				self.state = if frame_size > 1 {
					// don't bother moving to run state if only one pixel in this run
					// also, leaving this method in state Run(0, _) is a logic error
					DecodeState::Run(frame_size - 1, mode)
				}
				else {
					DecodeState::Pending
				};
				Some(Ok(mode))
			}
		};

		got.or_else(|| {
			let next = self.source.next();
			self.next_with_pulled(next)
		})
	}

	fn next_from_existing(&mut self) -> Option<<Self as Iterator>::Item> {

		let (to_return, next_state) = match self.state {
			DecodeState::Pending => unreachable!(),
			DecodeState::Done    => { return None; },
			DecodeState::Run(ref mut remaining, ref run_mode) => {
				*remaining -= 1;
				(*run_mode, if *remaining == 0 {Some(DecodeState::Pending)} else {None})
			},
			DecodeState::Frame(ref mut remaining, ref mut stage, ref mut stage_size) => {
				let got_bit = (*stage & 0x80) != 0;
				*stage = (*stage & 0x7f) << 1;
				*stage_size -= 1;
				*remaining -= 1;
				(got_bit, if *remaining == 0 {Some(DecodeState::Pending)} else {None})
			}
		};

		if let Some(next_state) = next_state {
			self.state = next_state;
		}

		Some(Ok(to_return))
	}

}

fn byte_to_run_size(byte: u8) -> u8 {
	let byte = byte & 0x3f;
	if byte == 0 { 0x40 } else { byte }
}

fn byte_to_frame_size(byte: u8) -> u8 {
	if byte == 0 { 0x80 } else { byte }	
}

#[derive(Debug)]
enum DecodeState {
	Pending, // should pull
	Run(u8, bool), // run count, is_set
	Frame(u8, u8, u8), // frame pixel count, stage contents, stage size
	Done, // iteration complete
}

#[cfg(test)]
mod test_decoder {

	macro_rules! decoder {
		( $($e:expr),* ) => {
			{
				let v = vec![$( $e )*];
				super::decode(v.into_iter())
			}
		}
	}

	#[test]
	fn empty_input() {
		let mut result = decoder![];

		assert_eq!(result.next(), None);
		assert_eq!(result.next(), None);
	}

	#[test]
	fn single_run_set() {
		for i in 0..0x3fu8 {
			let run_size = super::byte_to_run_size(i);
			let mut result = decoder![i+0xc0];
			for _ in 0..run_size {
				assert_eq!(result.next(), Some(Ok(true)));
			}
			assert_eq!(result.next(), None);
		}
	}

}
