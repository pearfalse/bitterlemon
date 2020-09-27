//! An RLE-based encoder and decoder for bit streams.
//!
//! # Examples
//!
//! ## In-memory version
//!
//! The simplest interface to `bitterlemon` is the `encode` function, which you can use
//! if your bit source is an iterator of `bool`s:
//!
//! ```
//! use std::iter;
//!
//! // Create input bit stream from a variety of different generators
//! // With standard bit packing, this takes 17 bytes
//! let set1 = iter::repeat(true).take(50); // 50x `true`
//! let set2 = iter::repeat(false).take(51); // 51x `false`
//! let set3 = [true, false].iter().copied().cycle().take(33); // alternate for 33 bits
//! let input = set1.chain(set2).chain(set3);
//!
//! let output : Vec<_> = bitterlemon::encode(input).collect();
//! // Bitterlemon gets this down to 8 bytes
//! assert_eq!(b"\xf2\xb3\x21\x55\x55\x55\x55\x01", output.as_slice());
//! ```
//!
//! Decoding is similar, except that not all byte inputs produce a valid output:
//!
//! ```
//! let encoded = b"\xc8\x08\x92";
//! let decoded : Result<Vec<_>, _> = bitterlemon::decode(encoded.iter().copied())
//!     .collect();
//!
//! assert!(decoded.is_ok());
//! let decoded = decoded.unwrap();
//! assert_eq!(&[
//!     true, true, true, true, true, true, true, true,
//!     true, false, false, true, false, false, true, false,
//! ], &*decoded);
//! ```

pub mod decode;

// 0.3
mod encoding;
mod run_buffer;

pub use encoding::encode;

pub(crate) const MAX_RUN_SIZE: u8 = 64;
pub(crate) const MAX_FRAME_SIZE: u8 = 128;

pub use decode::{
	decode,
	Decoder,
	DecodeResult,
	Error,
};


/// Represents a single run.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum Run {
	/// Run is of `1`-valued bits
	Set(u8),
	/// Run is of `0`-valued bits
	Clear(u8)
}

impl Run {
	pub fn new(bit: bool) -> Run {
		if bit {
			Run::Set(1)
		} else {
			Run::Clear(1)
		}
	}

	pub fn bit(&self) -> bool {
		matches!(*self, Run::Set(_))
	}

	pub fn len(&self) -> u8 {
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

	pub fn increment(&mut self) {
		let len = self.len_mut();
		debug_assert!(*len < crate::MAX_RUN_SIZE);
		*len = len.wrapping_add(1);
	}
}

impl From<Run> for u8 {
	fn from(src: Run) -> u8 {

		let high_bits: u8 = match src {
			Run::Set  (_) => 0xc0,
			Run::Clear(_) => 0x80,
		};

		let run_size_encoded: u8 = match src.len() {
			MAX_RUN_SIZE => 0, // encode max run len as 0
			n if n > MAX_RUN_SIZE => unreachable!("run too long: {:?}", src),
			other => other, // all valid non-0 values encode as themselves
		};

		high_bits | run_size_encoded
	}
}
