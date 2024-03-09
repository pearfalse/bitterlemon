use core::{
	borrow::Borrow,
	mem::replace,
};

use crate::{
	bl,
	BitDirection,
};

#[derive(Debug)]
pub(crate) struct ByteUnpack {
	stage: u8,
	bit: Option<bl::Bit>,
	advance: fn(bl::Bit) -> (bl::Bit, bool),
}

impl ByteUnpack {
	pub(crate) fn new(byte: u8, direction: BitDirection) -> ByteUnpack {
		ByteUnpack {
			stage: byte,
			bit: Some(direction.start_bit()),
			advance: direction.advance_function(),
		}
	}
}

impl Iterator for ByteUnpack {
	type Item = bool;

	fn next(&mut self) -> Option<Self::Item> {
		let bit = self.bit?;
		let mask = 1 << *bit;
		let r = (self.stage & mask) != 0;
		let (new_bit, wrapped) = (self.advance)(bit);
		self.bit = match wrapped {
			false => Some(new_bit),
			true => None,
		};
		Some(r)
	}
}

#[derive(Debug)]
pub(crate) struct SliceUnpack<S> {
	source: S,
	stage: Option<ByteUnpack>,
	direction: BitDirection,
}

impl<T: Borrow<u8>, S: IntoIterator<Item = T>> SliceUnpack<S> {
	pub(crate) fn new(source: S, direction: BitDirection) -> SliceUnpack<S::IntoIter> {
		let mut source = source.into_iter();
		let first_byte = source.next().map(|t|
			ByteUnpack::new(*t.borrow(), direction));
		SliceUnpack {
			source,
			stage: first_byte,
			direction,
		}
	}
}

impl<T: Borrow<u8>, S: Iterator<Item = T>> Iterator for SliceUnpack<S> {
	type Item = bool;

	fn next(&mut self) -> Option<Self::Item> {
		loop {
			if let next @ Some(_) = self.stage.as_mut()?.next() {
				return next;
			}
			self.stage = self.source.next().map(|n|
				ByteUnpack::new(*n.borrow(), self.direction));
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

	type Collect1 = arrayvec::ArrayVec<bool, 8>;
	type Collect2 = arrayvec::ArrayVec<bool, 16>;

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


#[derive(Debug)]
pub(crate) struct BytePack {
	stage: u8,
	mask: bl::Bit,
}

impl BytePack {
	pub(crate) fn new() -> BytePack {
		BytePack {
			stage: 0u8,
			mask: bl::Bit::START,
		}
	}

	pub(crate) fn pack(&mut self, bit: bool) -> Option<u8> {
		self.stage |= (bit as u8) << *self.mask;
		let (new_mask, wrapped) = self.mask.dec();
		self.mask = new_mask;
		if wrapped {
			Some(replace(&mut self.stage, 0))
		} else {
			None
		}
	}

	pub(crate) fn flush(self) -> Option<u8> {
		Some(self.stage).filter(|_| self.mask != bl::Bit::START)
	}
}


#[cfg(test)]
mod test_byte_packing {
	use super::*;

	const SIXTEEN_BITS: [bool; 16] = [
		true, false, false, true,
		false, false, true, false,
		false, true, false, false,
		true, false, true, false,
	];

	type Collect1 = arrayvec::ArrayVec<u8, 1>;

	#[test]
	fn one_byte() {
		for (half, source, dst) in [
			( "first", &SIXTEEN_BITS[..8], 0x92),
			("second", &SIXTEEN_BITS[8..], 0x4a),
		].iter().copied() {
			println!("{} half", half);
			let mut bp = BytePack::new();
			let generated = source.iter().copied()
			.filter_map(|b| bp.pack(b))
			.collect::<Collect1>();
			assert_eq!(&[dst], &*generated);
		}
	}
}
