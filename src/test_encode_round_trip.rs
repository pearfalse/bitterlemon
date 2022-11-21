//! Encodes a boolean stream, then decodes it, checking that the output matches the input

use arrayvec::ArrayVec;
use random::Source;

use crate::{
	encode,
	decode,
};

fn from_digits(s: &'static [u8]) -> Vec<bool> {
	s.iter().map(|&c| c == b'1').collect()
}

fn case(s: &'static [u8]) {
	let source = from_digits(s);

	let encoder = encode::encode(source.iter().map(|&b| b));
	let result : Result<Vec<_>, _> = decode::decode(encoder).collect();

	assert!(result.is_ok());
	let result = result.unwrap();

	assert_eq!(source, result);
}

#[test]
fn single_run() {
	case(b"11111111");
	case(b"00000000");
}

#[test]
fn single_frame() {
	case(b"101010101010");
	case(b"011011011011");
}

#[test]
fn random_kilobyte() {
	let mut rand_source = random::Xorshift128Plus::new([0x426173637265656e, 0x0123456789abcdef]);

	const DATA_SIZE : usize = 1024;
	const RUN_COUNT : usize = 5000;

	for _ in 0..RUN_COUNT {
		let mut source = ArrayVec::<_, DATA_SIZE>::new();

		for _ in 0..(DATA_SIZE / 64) {
			let rand = rand_source.read_u64();
			for i in 0..64 {
				source.push(rand & (1u64 << i) != 0);
			}
		}

		assert_eq!(source.len(), source.capacity());
	}
}
