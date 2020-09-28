//! Decodes a Bitterlemon-encoded byte stream into its original bit stream.

use crate::{
	Run,
	Bit,
	MAX_FRAME_SIZE,
	MAX_RUN_SIZE,
};

use std::iter::FusedIterator;

#[derive(Debug)]
enum Contents {
	Frame {
		stage: Option<u8>,
		stage_bit: Bit,
		bits_remaining: u8,
	},
	Run(Run),
}

#[derive(Debug, Default)]
pub struct Decoder {
	stage: [u8; (MAX_FRAME_SIZE / 8) as usize],
	contents: Option<Contents>,
}

impl Decoder {
	pub fn new() -> Decoder {
		Decoder::default()
	}

	pub fn raw_update(&mut self, input: &mut Option<u8>)
	-> Result<Option<bool>, TruncatedInputError> {
		dbg!(&self.contents, &input);
		let contents: &mut Contents = match self.contents {
			Some(ref mut c) => c,
			None => match input.take() {
				Some(b) if b < 0x80 => {
					// new frame time
					self.contents = Some(Contents::Frame {
						stage: None,
						stage_bit: Bit::Bit0,
						bits_remaining: match b {
							0 => MAX_FRAME_SIZE,
							n => n,
						},
					});
					return Ok(None); // need another byte
				},
				Some(b) => {
					let mut r = Run::new(b & 0x40 != 0);
					r.set_len(match b & 0x3f {
						0 => MAX_RUN_SIZE,
						n => n,
					});

					self.contents = Some(Contents::Run(r));
					match self.contents {
						Some(ref mut guaranteed) => guaranteed,
						_ => unsafe {
							// checked immediately after assignment to known value
							::std::hint::unreachable_unchecked()
						}
					}
				},
				None => return Ok(None)
			}
		};

		match *contents {
			Contents::Frame {
				stage: ref mut maybe_stage,
				ref mut stage_bit,
				ref mut bits_remaining
			} => {
				let stage = match *maybe_stage {
					Some(ref mut st) => st,
					None => {
						// next byte of stage, please
						let next_stage = input.take()
						.ok_or_else(|| TruncatedInputError::from_bits(*bits_remaining))?;
						// assign to *maybe_stage, and reborrow
						*maybe_stage = Some(next_stage);
						match maybe_stage {
							Some(yes) => yes,
							None => unsafe {
								std::hint::unreachable_unchecked()
							}
						}
					}
				};
				// grab next bit from frame
				let bit = (*stage & (1 << **stage_bit)) != 0;
				let (new_bit, wrapped) = stage_bit.inc();
				*stage_bit = new_bit;
				*bits_remaining -= 1;
				if *bits_remaining == 0 {
					*dbg!(&mut self.contents) = None;
				} else if wrapped {
					*maybe_stage = None; // clear stage
				}
				Ok(Some(bit))
			},
			Contents::Run(ref mut run) => {
				let bit = run.bit();
				if let Some(smaller) = run.try_dec() {
					*run = smaller;
				} else {
					self.contents = None; // run is exhausted
				}
				Ok(Some(bit))
			}
		}
	}
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TruncatedInputError {
	pub bits_lost: u8,
	pub bytes_expected: u8,
}

impl TruncatedInputError {
	fn from_bits(bits: u8) -> TruncatedInputError {
		TruncatedInputError {
			bits_lost: bits,
			bytes_expected: (bits + 7) / 8,
		}
	}
}

#[derive(Debug)]
pub struct IterableDecoder<S> {
	source: S,
	decoder: Decoder,
	cached: Option<u8>,
}

impl<S: Iterator<Item = u8>> Iterator for IterableDecoder<S> {
	type Item = Result<bool, TruncatedInputError>;

	fn next(&mut self) -> Option<Self::Item> {
		loop {
			if self.cached.is_none() {
				self.cached = self.source.next();
			}

			let has_input = self.cached.is_some();
			let _proof = self.cached;
			match self.decoder.raw_update(&mut self.cached) {
				Ok(Some(bit)) => break Some(Ok(bit)),
				Err(e) => break Some(Err(e)),
				Ok(None) if has_input => {
					// frame header
					debug_assert!(_proof.unwrap() < 0x80);

					continue;
				},
				Ok(None) => return None // assume EOF
			}
		}
	}
}

impl<S: Iterator<Item = u8>> FusedIterator for IterableDecoder<S> { }


pub fn decode<S: IntoIterator<Item = u8>>(source: S)
-> IterableDecoder<S::IntoIter> {
	IterableDecoder {
		source: source.into_iter(),
		decoder: Decoder::new(),
		cached: None,
	}
}

#[cfg(test)]
mod test_iterable_decoder {
	macro_rules! decoder {
		( $($e:expr),* ) => {
			{
				let v = vec![$( $e, )*];
				super::decode(v)
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
		for i in 0x80..0xbfu8 {
			let run_size = match i & 0x3f {
				0 => super::MAX_RUN_SIZE,
				n => n,
			};
			let mut iter = decoder![i+top_bits];
			for _ in 0..run_size {
				assert_eq!(iter.next(), Some(Ok(mode)));
			}
			assert_eq!(iter.next(), None);
		}
	}

	#[test]
	fn single_run_clear() {
		single_run_impl(0x00, false)
	}

	#[test]
	fn single_run_set() {
		single_run_impl(0x40, true)
	}

	#[test]
	fn single_byte_frame() {
		let case = |byte_in: u8, bool_out: bool| {
			eprintln!("CASE: {:02x}", byte_in);
			let mut iter = decoder![0x01, byte_in];
			assert_eq!(Some(Ok(bool_out)), iter.next());
		};

		case(0xff, true);
		case(0x00, false);
		case(0x01, true);
		case(0xfe, false);
	}

	#[test]
	fn full_byte_frame() {
		let mut iter = decoder![0x08, 0xaa];

		let mut expected = false;
		for _ in 0..8 {
			assert_eq!(iter.next(), Some(Ok(expected)));
			expected = !expected;
		}

		assert_eq!(iter.next(), None);
	}

	#[test]
	fn two_byte_frame() {
		let mut iter = decoder![0x0f, 0xaa, 0xaa];

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

		case(&[0xc1, 0x10, 0xaa, 0xaa, 0x81], 18, true);
		case(&[0x81, 0x10, 0x55, 0x55, 0xc1], 18, false);
		case(&[0x08, 0x55, 0xc1, 0x08, 0xaa], 17, true);
		case(&[0x08, 0xaa, 0x81, 0x08, 0x55], 17, false);
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
				super::TruncatedInputError {
					bits_lost: pl, bytes_expected: bm,
				} => {
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

