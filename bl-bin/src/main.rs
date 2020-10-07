use bitterlemon as bl;
use gumdrop;
use gumdrop::Options;

mod packing;
use packing::*;

use std::{
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
	NotSupported(&'static str),
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
			NotSupported(feature) => write!(f,
				"sorry, feature '{}' is not supported yet", feature),
		}
	}
}

fn main() {
	let stdin = io::stdin();
	let stdout = io::stdout();
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

	std::process::exit(match main2(opts, stdin.lock(), stdout.lock()) {
		Ok(()) => 0,
		Err(e) => {
			let mut lock = stderr.lock();
			writeln!(lock, "{}", e).ok();
			1
		}
	});
}

fn main2<'a, In: BufRead + 'a, Out: Write + 'a>(
	args: BlOptions,
	mut stdin: In,
	mut stdout: Out,
) -> Result<(), Error> {
	type Input<'a> = dyn io::BufRead + 'a;
	type Output<'a> = dyn io::Write + 'a;

	let (io_input_error, io_output_error) = (
		|e: io::Error| Error::Io(args.input_file.clone(), e),
		|e: io::Error| Error::Io(args.output_file.clone(), e),
	);

	let mut input_file_fs;
	let input_file = if args.input_file == STDIO_ARG {
		&mut stdin as &mut Input<'a>
	} else {
		input_file_fs = OpenOptions::new()
			.read(true)
			.open(&args.input_file)
			// StdinLock impls BufReader, but File doesn't
			.map(|f| io::BufReader::with_capacity(BUF_SIZE, f))
			.map_err(io_input_error)?;
		&mut input_file_fs as &mut Input<'a>
	};

	let mut output_file_fs;
	// We need buffered writes on all outputs
	let mut output_file = io::BufWriter::with_capacity(
		BUF_SIZE, if args.output_file == STDIO_ARG {
			&mut stdout as &mut Output<'a>
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
	let bit_direction = if args.msb_first { BitDirection::MsbFirst }
	else { BitDirection::LsbFirst };

	if ! args.decode {
		'enc: loop {
			let buf = input_file.fill_buf().map_err(io_input_error)?;
			if buf.is_empty() {
				break 'enc;
			}
			let buf_len = buf.len();
			let mut bits = SliceUnpack::new(buf, bit_direction);

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
	}
	else {
		return Err(Error::NotSupported("decoding"));
	}

	output_file.flush().map_err(io_output_error)
}

#[cfg(test)]
mod test_main {
	use super::*;

	#[test]
	fn decoding_not_supported_yet() {
		// it will be eventually, promise
		let result = main2(BlOptions {
			help: false,
			input_file: String::from("input"),
			output_file: String::from("output"),
			msb_first: false,
			decode: true,
		}, std::io::empty(), DummyIoWrite);

		assert!(matches!(
			result, Err(Error::NotSupported(_))
		));
	}

	#[test]
	fn encode() {

	}
}


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub(crate) enum BitDirection {
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


#[cfg(test)]
struct DummyIoWrite;

#[cfg(test)]
impl Write for DummyIoWrite {
	fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
		Ok(buf.len())
	}

	fn flush(&mut self) -> io::Result<()> {
		Ok(())
	}
}
