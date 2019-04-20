# bitterlemon

Bitterlemon is a data format and Rust library crate to efficiently store bit sequences in byte streams using run-length encoding (RLE). Bitterlemon will pack contiguous bit runs into compact RLE representations where it can, and fall back to standard byte packing where RLE can't improve the compression factor. There are no limits or compression performance penalties related to input or output size.

## Usage

Add `bitterlemon` as a dependency in your `Cargo.toml` file:

	[dependencies]
	bitterlemon = ">= 0.2.0"

API documentation is hosted on [docs.rs](https://docs.rs/bitterlemon).

## License

Bitterlemon is released under terms of the MIT license. See the file `LICENSE` for more.

## Data format

Bitterlemon data is a sequence of _runs_ and _frames_, with runs further split into _set runs_ and _clear runs_. A run is a compacted representation of a contiguous series of bits, all with the same value. Clear runs hold a sequence of 0s, and Set runs a sequence of 1s. A _frame_ is a sized sequence of bits packed into a byte, where the bit value changes too often for runs to be of any benefit. Runs can be 1–64 bytes in length, while frames can be 1–128 bytes.

### Byte values

Runs are encoded as single bytes with the following bit representation:

	MSB  1Tnnnnnn  LSB

`T` is the run type (0 for clear runs, 1 for set runs) and `nnnnnn` is the run length (runs of 64 are encoded as `000000`).

Frames are encoded as follows:

	0LLLLLLL bbbbbbbb bbbbbbbb … bbbbbbbb

`LLLLLLL` represents the number of _bits_ in the frame (a length of 128 is encoded as `0000000`). The following bytes are the bits contained within the frame, starting with the MSB of the first subsequent byte. If a frame length does not align to a byte boundary, it should be padded to 0 bits to complete the last byte. In all cases, the length of the leading byte is the authority on how many bits have been encoded.

The number of bytes needed to encode a frame can be worked out as follows:

	frame_size_in_bytes = ((number_of_bits + 7) >> 3) + 1

### Implementation details

The reference encoder first converts the input bit stream to a series of runs, each 1 to 64 bits in length. There is then a second pass that replaces some of these runs with frames, where it would take fewer bytes to do so.
