//! Decodes a Bitterlemon-encoded byte stream into its original bit stream.

use std::iter::Iterator;
use std::result;

/// Decodes a Bitterlemon byte stream into an iterator of `bool`s.
/// `source` can be any iterator that yields `u8` values.
///
/// # Errors
///
/// Unlike encoding, decoding has a chance of failure. The exposed iterator
/// will return a [`Result`] object to handle the possibility of an invalid
/// input stream. The `Ok` value in this case is of type `bool`.
///
/// [`Result`]: type.Result.html
pub fn decode<S>(input: S) -> Decoder<S>
where S : Iterator<Item=u8> {
	Decoder::<S> {
		source: input,
		state: DecodeState::Pending,
	}
}

/// Manages the state for decoding a Bitterlemon byte stream.
///
/// To perform a decoding, see [`decode`](#fn.decode).
pub struct Decoder<S> {
	state: DecodeState,
	source: S,
}

/// Describes errors that can occur when decoding a Bitterlemon byte stream.
#[derive(Debug, PartialEq, Eq)]
pub enum Error {
	/// Input had too few bytes to cleanly decode. The associated values are:
	///
	/// * number of bits lost due to truncated input;
	/// * number of bytes still expected from the input.
	TruncatedInput(u8, u8),
}

/// Decode operations yield this on each iteration.
pub type Result = result::Result<bool, Error>;

impl<S> Iterator for Decoder<S>
where S : Iterator<Item=u8> {
	type Item = Result;

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
					let error_specifics = Error::TruncatedInput(remaining, (remaining + 7) >> 3);
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
					// don't bother moving to run state if only one bit in this run
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
	Frame(u8, u8, u8), // frame bit count, stage contents, stage size
	Done, // iteration complete
}

#[cfg(test)]
mod test_decoder {

	macro_rules! decoder {
		( $($e:expr),* ) => {
			{
				let v = vec![$( $e, )*];
				super::decode(v.into_iter())
			}
		}
	}

	#[test]
	fn empty_input() {
		let mut iter = decoder![];

		assert_eq!(iter.next(), None);
		assert_eq!(iter.next(), None);
	}

	fn single_run_impl(top_bits: u8, mode: bool) {
		for i in 0..0x3fu8 {
			let run_size = super::byte_to_run_size(i);
			let mut iter = decoder![i+top_bits];
			for _ in 0..run_size {
				assert_eq!(iter.next(), Some(Ok(mode)));
			}
			assert_eq!(iter.next(), None);
		}
	}

	#[test]
	fn single_run_clear() {
		single_run_impl(0x80, false)
	}

	#[test]
	fn single_run_set() {
		single_run_impl(0xc0, true)
	}

	#[test]
	fn single_byte_frame() {
		let case = |byte_in: u8, bool_out: bool| {
			let mut iter = decoder![0x01, byte_in];
			assert_eq!(iter.next(), Some(Ok(bool_out)));
		};

		case(0xff, true);
		case(0x00, false);
		case(0x80, true);
		case(0x7f, false);
	}

	#[test]
	fn full_byte_frame() {
		let mut iter = decoder![0x08, 0x55];

		let mut expected = false;
		for _ in 0..8 {
			assert_eq!(iter.next(), Some(Ok(expected)));
			expected = !expected;
		}

		assert_eq!(iter.next(), None);
	}

	#[test]
	fn two_byte_frame() {
		let mut iter = decoder![0x0f, 0x55, 0x55];

		let mut expected = false;
		for _ in 0..15 {
			assert_eq!(iter.next(), Some(Ok(expected)));
			expected = !expected;
		}

		assert_eq!(iter.next(), None);
	}

	#[test]
	fn alternate_runs_frames() {
		let case = |bytes: &[u8], count: usize, first_output: bool| {
			let mut iter = super::decode(bytes.iter().map(|&b| b));

			let mut expected = first_output;
			for _ in 0..count {
				assert_eq!(iter.next(), Some(Ok(expected)));
				expected = !expected;
			}

			assert_eq!(iter.next(), None);
		};

		case(&[0xc1, 0x10, 0x55, 0x55, 0x81], 18, true);
		case(&[0x81, 0x10, 0xaa, 0xaa, 0xc1], 18, false);
		case(&[0x08, 0xaa, 0xc1, 0x08, 0x55], 17, true);
		case(&[0x08, 0x55, 0x81, 0x08, 0xaa], 17, false);
	}

	#[test]
	fn error_on_frame_cutoff() {
		let case = |bytes: &[u8], bits_lost: u8, bytes_missing: u8| {
			let mut iter = super::decode(bytes.iter().map(|&b| b));

			let ok_count = (bytes.len() - 1) * 8;
			for _ in 0..ok_count {
				assert!(match iter.next() {
					Some(Ok(_)) => true,
					_ => false
				});
			}

			let error = iter.next();
			assert!(error.is_some());
			let error = error.unwrap();

			assert!(error.is_err());
			match error.unwrap_err() {
				super::Error::TruncatedInput(pl, bm) => {
					assert_eq!(pl, bits_lost);
					assert_eq!(bm, bytes_missing);
				}
			};
		};

		case(&[0x01], 1, 1);
		case(&[0x02], 2, 1);
		case(&[0x00], 0x80, 0x10);
		case(&[0x09, 0xff], 1, 1);
		case(&[0x7f, 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14], 7, 1);
	}

	#[test]
	fn next_none_idempotence() {
		let src = &[0xc1u8];
		let mut iter = super::decode(src.iter().map(|&b| b));

		assert!(iter.next().is_some());

		for _ in 0..20 {
			assert_eq!(iter.next(), None);
		}
	}
}
