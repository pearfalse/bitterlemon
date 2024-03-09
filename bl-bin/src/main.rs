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

	#[options(help = "read input LSB-first", long = "lsb-first", no_short, default = "false")]
	lsb_first: bool,
}

impl BlOptions {
	#[inline(always)]
	const fn wants_encode(&self) -> bool { ! self.decode }
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
	let output_file = if args.output_file == STDIO_ARG {
			&mut stdout as &mut Output<'a>
	} else {
		output_file_fs = OpenOptions::new()
			.write(true)
			.truncate(true)
			.create(true)
			.open(&args.output_file)
			.map_err(io_output_error)?;
		&mut output_file_fs as &mut Output<'a>
	};

	let mut obuf = Vec::with_capacity(BUF_SIZE);
	macro_rules! obuf {
		() => {
			if obuf.len() == obuf.capacity() {
				obuf!(flush);
			}
		};
		(flush) => {{
			output_file.write_all(&*obuf).map_err(io_output_error)?;
			obuf.clear();
		}};
		($byte:expr) => {
			obuf.push($byte);
			obuf!();
		};
		(maybe $obyte:expr) => {
			if let Some(b) = $obyte {
				obuf.push(b);
			}

			obuf!();
		};
	}

	let mut decoder = bl::Decoder::new();

	let bit_direction = if args.lsb_first { BitDirection::LsbFirst }
	else { BitDirection::MsbFirst };

	if args.wants_encode() {
		let mut encoder = bl::Encoder::new();
		'enc: loop {
			let buf = input_file.fill_buf().map_err(io_input_error)?;
			if buf.is_empty() {
				break 'enc;
			}
			let buf_len = buf.len();
			// give us the input buffer as an iterator of bits
			let mut bits = SliceUnpack::new(buf, bit_direction);

			while let Some(bit) = bits.next() {
				// pushing a new bit might have given us a full byte to write out
				let enc_byte = match encoder.update(bit) {
					Some(byte) => byte,
					None => continue
				};

				obuf!(enc_byte);
			}
			input_file.consume(buf_len);
		}
		// input is empty, flush encoder
		for byte in encoder.flush() {
			obuf!(byte);
		}

		obuf!(flush);
	}
	else {
		// this converts our decoded bits into bytes for i/o
		let mut packer = BytePack::new();
		'blocks: loop {
			let buf = input_file.fill_buf().map_err(io_input_error)?;
			if buf.is_empty() {
				break 'blocks;
			}
			let buf_len = buf.len();
			// turn input buffer into an iterator of bytes
			let mut buf = buf.into_iter().copied();

			let mut stage = None;

			// we need to loop until all input bytes are consumed, *and* no more output bits appear
			'bits: loop {
				if stage.is_none() {
					// input byte was consumed; try refilling it
					stage = buf.next();

					if stage.is_none() {
						input_file.consume(buf_len);
						// flush decoder
						while let Some(next_bit) = decoder.update(&mut stage)
						.map_err(Error::Decode)? {
							obuf!(maybe packer.pack(next_bit));
						}

						continue 'blocks;
					}
				}

				let next_bit = match decoder.update(&mut stage).map_err(Error::Decode)? {
					Some(b) => b,
					None => {
						// decoder quietly consumed our input byte (frame header); run again
						continue 'bits
					}
				};

				obuf!(maybe packer.pack(next_bit));
			}
		}

		obuf!(maybe packer.flush());
		obuf!(flush);
	}

	debug_assert!(obuf.is_empty(), "buffer still contains {} byte(s)", obuf.len());
	output_file.flush().map_err(io_output_error)
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
