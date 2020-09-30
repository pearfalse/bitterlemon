use bitterlemon as bl;
use gumdrop;
use gumdrop::Options;

use std::{
	borrow::Borrow,
	fmt,
	fs::OpenOptions,
	io,
	io::{Write, BufRead},
};

static STDIO_ARG: &'static str = "-";
const BUF_SIZE: usize = 4096;

#[derive(Debug, Options)]
struct BlOptions {
	#[options(free, required, help = "input file")]
	input_file: String,

	#[options(free, required, help = "output file")]
	output_file: String,

	#[options(help = "show this help message", short = "h", long = "help")]
	help: bool,

	#[options(help = "decode bitterlemon input", short = "d")]
	decode: bool,

	#[options(help = "use MSB-first byte format", long = "msb-first", no_short)]
	msb_first: bool,
}

#[derive(Debug)]
enum Error {
	Io(String, io::Error),
	Decode(bl::TruncatedInputError), // encoding is infallible
}

impl fmt::Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		use Error::*;
		match *self {
			Io(ref problem, ref ioe) => write!(f, "{} I/O error: {}", problem, ioe),
			Decode(ref ble) => {
				write!(f, "input stops too early (expected another {} bytes, \
				lost {} bits of output", ble.bytes_expected, ble.bits_lost)
			},
		}
	}
}

fn main() {
	let mut stdin = io::stdin();
	let mut stdout = io::stdout();
	let mut stderr = io::stderr();

	let args = std::env::args().skip(1).collect::<Vec<_>>();

	let opts = match BlOptions::parse_args(&args, gumdrop::ParsingStyle::StopAtFirstFree) {
		Ok(ref opts) if opts.help_requested() => {
			show_usage(&mut stderr)
		},
		Err(e) => {
			writeln!(stderr.lock(), "{}", e).ok();
			show_usage(&mut stderr)
		},
		Ok(opts) => {
			opts
		},
	};

	#[inline(never)]
	fn show_usage(stderr: &mut io::Stderr) -> ! {
		writeln!(stderr.lock(), "{}", BlOptions::usage()).ok();
		std::process::exit(2);
	}

	std::process::exit(match main2(opts, &mut stdin, &mut stdout) {
		Ok(()) => 0,
		Err(e) => {
			let mut lock = stderr.lock();
			writeln!(lock, "{}", e).ok();
			1
		}
	});
}

fn main2<'a>(args: BlOptions, stdin: &'a mut io::Stdin, stdout: &'a mut io::Stdout)
-> Result<(), Error> {
	type Input<'a> = dyn io::Read + 'a;
	type Output<'a> = dyn io::Write + 'a;

	let (io_input_error, io_output_error) = (
		|e: io::Error| Error::Io(args.input_file.clone(), e),
		|e: io::Error| Error::Io(args.output_file.clone(), e),
	);

	let (mut input_file_fs, mut input_file_stdio);
	let mut input_file = io::BufReader::with_capacity(
		BUF_SIZE,
		if args.input_file == STDIO_ARG {
			input_file_stdio = stdin.lock();
			&mut input_file_stdio as &mut Input<'a>
		} else {
			input_file_fs = OpenOptions::new()
				.read(true)
				.open(&args.input_file)
				.map_err(io_input_error)?;
			&mut input_file_fs as &mut Input<'a>
		}
	);

	let (mut output_file_fs, mut output_file_stdio);
	let mut output_file = io::BufWriter::with_capacity(
		BUF_SIZE,
		if args.output_file == STDIO_ARG {
			output_file_stdio = stdout.lock();
			&mut output_file_stdio as &mut Output<'a>
		} else {
			output_file_fs = OpenOptions::new()
				.write(true)
				.truncate(true)
				.create(true)
				.open(&args.output_file)
				.map_err(io_output_error)?;
			&mut output_file_fs as &mut Output<'a>
		}
	);

	let mut encoder = bl::Encoder::new();

	'chunks: loop {
		let buf = input_file.fill_buf().map_err(io_input_error)?;
		if buf.is_empty() {
			break 'chunks;
		}
		let buf_len = buf.len();
		let mut bits = SliceUnpackLsbFirst::new(buf);

		while let Some(bit) = bits.next() {
			if let Some(output) = encoder.update(bit) {
				output_file.write(&[output][..]).map_err(io_output_error)?;
			}
		}
		input_file.consume(buf_len);
	}

	// input is empty
	for trailing_byte in encoder.flush() {
		output_file.write(&[trailing_byte][..]).map_err(io_output_error)?;
	}

	Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
enum BitDirection {
	LsbFirst,
	MsbFirst,
}

impl BitDirection {
	fn start_bit(self) -> bl::Bit {
		match self {
			BitDirection::LsbFirst => bl::Bit::Bit0,
			BitDirection::MsbFirst => bl::Bit::Bit7,
		}
	}

	fn advance_function(self) -> fn(bl::Bit) -> (bl::Bit, bool) {
		match self {
			BitDirection::LsbFirst => bl::Bit::inc,
			BitDirection::MsbFirst => bl::Bit::dec,
		}
	}
}

#[derive(Debug)]
struct ByteUnpack {
	stage: u8,
	bit: Option<bl::Bit>,
	direction: BitDirection,
}

impl ByteUnpack {
	fn new(byte: u8, direction: BitDirection) -> ByteUnpack {
		ByteUnpack {
			stage: byte,
			bit: Some(direction.start_bit()),
			direction,
		}
	}
}

impl Iterator for ByteUnpack {
	type Item = bool;

	fn next(&mut self) -> Option<Self::Item> {
		self.bit.map(|bit| {
			let mask = 1 << *bit;
			let r = (self.stage & mask) != 0;
			let (new_bit, wrapped) = (self.direction.advance_function())(bit);
			self.bit = match wrapped {
				false => Some(new_bit),
				true => None,
			};
			r
		})
	}
}

#[derive(Debug)]
struct SliceUnpack<S> {
	source: S,
	stage: Option<ByteUnpack>,
	direction: BitDirection,
}

impl<T: Borrow<u8>, S: IntoIterator<Item = T>> SliceUnpack<S> {
	fn new(source: S, direction: BitDirection) -> SliceUnpack<S::IntoIter> {
		let mut source = source.into_iter();
		let first_byte = source.next().map(|t| new_stage(t, direction));
		SliceUnpack {
			source,
			stage: first_byte,
			direction,
		}
	}
}

fn new_stage<T: Borrow<u8>>(t: T, direction: BitDirection) -> ByteUnpack {
	ByteUnpack::new(*t.borrow(), direction)
}

impl<T: Borrow<u8>, S: Iterator<Item = T>> Iterator for SliceUnpack<S> {
	type Item = bool;

	fn next(&mut self) -> Option<Self::Item> {
		loop {
			if let next @ Some(_) = self.stage.as_mut()?.next() {
				return next;
			}
			self.stage = self.source.next().map(|n| new_stage(n, self.direction));
		}
	}
}

#[cfg(test)]
mod test_byte_unpacking {
	use super::*;

	const SIXTEEN_BITS: [bool; 16] = [
		true, false, false, true,
		false, false, true, false,
		false, true, false, false,
		true, false, true, false,
	];

	type Collect1 = arrayvec::ArrayVec<[bool; 8]>;
	type Collect2 = arrayvec::ArrayVec<[bool; 16]>;

	#[test]
	fn one_byte() {
		for (source1, source2, direction) in [
			(0x49, 0x52, BitDirection::LsbFirst),
			(0x92, 0x4a, BitDirection::MsbFirst),
		].iter().copied() {
			println!("first half, {:?}", direction);
			let generated = ByteUnpack::new(source1, direction)
			.collect::<Collect1>();
			assert_eq!(&SIXTEEN_BITS[..8], &*generated);

			println!("second half, {:?}", direction);
			let generated = ByteUnpack::new(source2, direction)
			.collect::<Collect1>();
			assert_eq!(&SIXTEEN_BITS[8..], &*generated);
		}
	}

	#[test]
	fn two_bytes() {
		for (source, direction) in [
			([0x49, 0x52], BitDirection::LsbFirst),
			([0x92, 0x4a], BitDirection::MsbFirst),
		].iter().copied() {
			println!("direction: {:?}", direction);
			let generated = SliceUnpack::new(&source[..], direction)
			.collect::<Collect2>();
			assert_eq!(&SIXTEEN_BITS[..], &*generated);
		}
	}
}
