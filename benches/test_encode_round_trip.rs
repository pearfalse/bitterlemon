//! Encodes a boolean stream, then decodes it, checking that the output matches the input

use arrayvec;
use random::Source;

use criterion::{criterion_group, criterion_main, Criterion};
use bitterlemon::{
	encode,
	decode,
};

fn random_kilobyte(c: &mut Criterion) {
	let mut rand_source = random::Xorshift128Plus::new([0x426173637265656e, 0x0123456789abcdef]);

	const DATA_SIZE : usize = 1024;

	c.bench_function("1kb round-trip", |b| {
		let mut source = arrayvec::ArrayVec::<[_; DATA_SIZE]>::new();

		for _ in 0..(DATA_SIZE / 64) {
			let rand = rand_source.read_u64();
			for i in 0..64 {
				source.push(rand & (1u64 << i) != 0);
			}
		}

		assert_eq!(source.len(), source.capacity());
		b.iter(|| {
			let encoder = encode(source.iter().copied());
			let decoder = decode(encoder);

			source.iter().map(|s| Ok(*s))
			.zip(decoder)
			.for_each(|(a, b)| assert_eq!(a, b));
		})
	});
}

criterion_group!(benches, random_kilobyte);
criterion_main!(benches);
