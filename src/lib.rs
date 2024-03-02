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
//! let encoded = b"\xc8\x08\x49";
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
//!
//! ## Truncated input
//!
//! Bitterlemon can detect truncated input, which is the only way that an arbitrary
//! input would not decode to a valid bit stream:
//!
//! ```
//! let encoded = b"\x13\xf6"; // uh oh, this needs another 2 bytes to hold another 11 bits
//! let result = bitterlemon::decode(encoded.iter().copied())
//!     .collect::<Result<Vec<_>, _>>();
//!
//! let error: bitterlemon::TruncatedInputError = result.unwrap_err();
//! assert_eq!(11, error.bits_lost);
//! assert_eq!(2, error.bytes_expected);
//! ```

#![deny(unsafe_op_in_unsafe_fn)]

// TODO: almost possible, just need to dummy out formatters via a feature flag
// #![cfg_attr(all(feature = "no_std", not(test)), no_std)]

mod encoding;
mod decoding;
mod run_buffer;

pub use encoding::{
	encode,
	Encoder,
};

pub(crate) const MAX_RUN_SIZE: u8 = 64;
pub(crate) const MAX_FRAME_SIZE: u8 = 128;

pub use decoding::{
	decode,
	Decoder,
	TruncatedInputError,
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

	pub fn set_len(&mut self, new_len: u8) {
		debug_assert!(new_len <= MAX_RUN_SIZE);
		*self.len_mut() = new_len;
	}

	fn len_mut(&mut self) -> &mut u8 {
		let len = match *self {
			Run::Set(ref mut x) => x,
			Run::Clear(ref mut x) => x,
		};
		debug_assert!(*len <= crate::MAX_RUN_SIZE);
		len
	}

	pub fn try_inc(self) -> Option<Run> {
		self.try_step_impl(MAX_RUN_SIZE, u8::wrapping_add)
	}

	pub fn try_dec(self) -> Option<Run> {
		self.try_step_impl(1, u8::wrapping_sub)
	}

	#[inline(always)]
	fn try_step_impl(self, limit: u8, step_func: fn(u8, u8) -> u8) -> Option<Run> {
		let mut new = self;
		let new_len = new.len_mut();
		if *new_len == limit {
			None
		}
		else {
			*new_len = step_func(*new_len, 1);
			Some(new)
		}
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

#[cfg(test)]
mod test_run {
	use super::Run;

	#[test]
	fn try_dec() {
		assert_eq!(Some(Run::Set(2)), Run::Set(3).try_dec());
		assert_eq!(None, Run::Set(1).try_dec());
	}
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
#[allow(dead_code)] // variants are used via `inc` and `dec`, which transmute
pub enum Bit {
	Bit0 = 0,
	Bit1 = 1,
	Bit2 = 2,
	Bit3 = 3,
	Bit4 = 4,
	Bit5 = 5,
	Bit6 = 6,
	Bit7 = 7,
}

impl From<Bit> for u8 {
	fn from(value: Bit) -> Self {
		*value.as_u8()
	}
}

impl Bit {
	pub fn inc(self) -> (Self, bool) {
		use Bit::*;
		if self == Bit7 {
			(Bit0, true)
		} else {
			(unsafe {
				// SAFETY: OOB case was checked above; all remaining values have a match here
				Self::from_u8_unchecked((self as u8).wrapping_add(1))
			}, false)
		}
	}

	pub fn dec(self) -> (Self, bool) {
		use Bit::*;
		if self == Bit0 {
			(Bit7, true)
		} else {
			(unsafe {
				// SAFETY: OOB case was checked above; all remaining values have a match here
				Self::from_u8_unchecked((self as u8).wrapping_sub(1))
			}, false)
		}
	}

	unsafe fn from_u8_unchecked(src: u8) -> Self {
		debug_assert!(src < 8);
		unsafe {
			// SAFETY: caller must ensure that `src` is in 0..8
			*((&src as *const u8).cast::<Self>())
		}
	}

	fn as_u8(&self) -> &u8 {
		unsafe {
			// SAFETY: this enum has repr(u8), and all values are defined
			&*(self as *const Self).cast::<u8>()
		}
	}
}

impl std::ops::Deref for Bit {
	type Target = u8;
	fn deref(&self) -> &Self::Target {
		self.as_u8()
	}
}

#[cfg(test)]
mod test_bit {
	use super::*;

	#[test]
	fn inc_dec() {
		use Bit::*;
		let values = [Bit0, Bit1, Bit2, Bit3, Bit4, Bit5, Bit6, Bit7];

		for idx in 0..=7 {
			let (expected, (new_val, did_wrap)) = (values[(idx + 1) & 7], values[idx].inc());
			assert_eq!(expected, new_val);
			assert!(did_wrap == (idx == 7));
		}

		for idx in 7..=0 {
			let (expected, (new_val, did_wrap)) = (values[idx], values[(idx + 1) & 7].dec());
			assert_eq!(expected, new_val);
			assert!(did_wrap == (idx == 0));
		}
	}
}
