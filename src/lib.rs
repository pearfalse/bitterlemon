//! An RLE-based encoder and decoder for bit streams.
//!
//! # Example
//!
//! ```
//! use std::iter;
//!
//! // Create input bit stream from a variety of different generators
//! // With standard bit packing, this takes 17 bytes
//! let tmp = &[true, false];
//! let input = iter::repeat(true).take(50)
//! 	.chain(iter::repeat(false).take(51))
//! 	.chain(tmp.iter().cloned().cycle().take(33));
//!
//! let output : Vec<_> = bitterlemon::encode(input).collect();
//! // Bitterlemon gets this down to 8 bytes
//! assert_eq!(b"\xf2\xb3\x21\xaa\xaa\xaa\xaa\x80", output.as_slice());
//!
//! let decoded : Result<Vec<_>, _>
//! 	= bitterlemon::decode(output.iter().map(|&b| b)).collect();
//! assert!(decoded.is_ok());
//! let decoded = decoded.unwrap();
//! assert_eq!(134, decoded.len());
//! ```

mod encode;
mod decode;

pub use encode::{encode,Encoder};
pub use decode::{decode,Decoder,Result,Error};