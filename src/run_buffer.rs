//! Basic ring buffer to hold runs for a potential frame.

use std::{
	fmt,
	mem::MaybeUninit,
	ptr,
};

use crate::{
	encode::Run,
	MAX_FRAME_SIZE,
};

pub(crate) struct RunBuffer {
	store: [MaybeUninit<Run>; Self::capacity() as usize],
	head: u8,
	tail: u8,
	len: u8,
}

impl fmt::Debug for RunBuffer {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		f.debug_struct("RunBuffer")
			.field("head", &self.head)
			.field("tail", &self.tail)
			.field("len" , &self.len)
			.finish()
	}
}

impl RunBuffer {
	pub fn new() -> RunBuffer {
		let store = unsafe {
			let transposed = MaybeUninit::<[Run; Self::capacity() as usize]>::uninit();
			std::mem::transmute(transposed)
		};

		RunBuffer {
			store,
			head: 0,
			tail: 0,
			len: 0,
		}
	}

	pub const fn capacity() -> u8 { MAX_FRAME_SIZE }
	const fn inc_mask() -> u8 { MAX_FRAME_SIZE - 1 }

	#[inline(always)]
	fn inc_ptr(ptr: u8) -> u8 {
		// will never wrap; max allowed input is 127
		ptr.wrapping_add(1) & Self::inc_mask()
	}

	#[inline(always)]
	fn dec_ptr(ptr: u8) -> u8 {
		// if underflows to 255, mask will make the result valid again
		ptr.wrapping_sub(1) & Self::inc_mask()
	}

	pub fn head(&self) -> Option<&Run> {
		if self.len > 0 {
			Some(unsafe { self.get_unchecked(self.head) })
		} else { None }
	}

	pub fn head_mut(&mut self) -> Option<&mut Run> {
		if self.len > 0 {
			Some(unsafe { self.get_unchecked_mut(self.head) })
		} else { None }
	}

	pub fn tail(&self) -> Option<&Run> {
		if self.len > 0 {
			Some(unsafe { self.get_unchecked(Self::dec_ptr(self.tail)) })
		} else { None }
	}

	pub fn tail_mut(&mut self) -> Option<&mut Run> {
		if self.len > 0 {
			Some(unsafe { self.get_unchecked_mut(Self::dec_ptr(self.tail)) })
		} else { None }
	}

	unsafe fn get_unchecked(&self, idx: u8) -> &Run{
		&*self.store[idx as usize].as_ptr()
	}

	unsafe fn get_unchecked_mut(&mut self, idx: u8) -> &mut Run {
		&mut *self.store[idx as usize].as_mut_ptr()
	}

	fn push(&mut self, run: Run) -> Result<(), Run> {
		if self.len == MAX_FRAME_SIZE { return Err(run); }

		unsafe {
			// checked in debug builds
			debug_assert!(self.tail < Self::capacity());
			let target = self.store.get_unchecked_mut(self.tail as usize).as_mut_ptr();
			ptr::write(target, run);
		}

		self.len += 1;
		self.tail = Self::inc_ptr(self.tail);

		Ok(())
	}

	fn pull(&mut self) -> Option<Run> {
		if self.len == 0 { return None; }

		let run = unsafe {
			// checked in debug builds
			debug_assert!(self.head < Self::capacity());
			let target = self.store.get_unchecked_mut(self.head as usize).as_ptr();
			// ptr::read isn't strictly necessary here, since Run is Copy and !Drop, but it
			// provides symmetry with `push`, and doesn't require the above invariants
			ptr::read(target)
		};

		self.len = self.len.wrapping_sub(1); // will not underflow; see top of method
		self.head = Self::inc_ptr(self.head);

		Some(run)
	}
}

#[cfg(test)]
mod test_run_buffer {
	use super::*;

	#[test]
	fn basic() {
		let mut rb = RunBuffer::new();
		assert_eq!(None, rb.pull());

		let runs = [
			Run::Set(1),
			Run::Set(2),
			Run::Clear(3),
			Run::Clear(4),
		];

		for run in runs.iter().copied() {
			rb.push(run).unwrap();
			assert_eq!(Some(&runs[0]), rb.head());
			assert_eq!(Some(&run), rb.tail());
		}

		let mut read_back = runs.iter().copied();
		while let expected @ Some(_) = read_back.next() {
			assert_eq!(expected, rb.pull());
		}

		assert_eq!(None, rb.pull()); // we emptied it
	}

	#[test]
	fn fill_aligned() {
		fill_impl(|_| {})
	}

	#[test]
	fn fill_unaligned() {
		fill_impl(|rb| {
			rb.push(Run::Clear(1)).unwrap();
			rb.push(Run::Clear(1)).unwrap();
			rb.push(Run::Clear(1)).unwrap();
			rb.pull();
			rb.pull();
			rb.pull();
		})
	}

	fn fill_impl(operation: fn(&mut RunBuffer)) {
		let mut rb = RunBuffer::new();
		operation(&mut rb);

		for run in all_run_types() {
			rb.push(run).unwrap();
		}

		assert_eq!(Err(Run::Set(2)), rb.push(Run::Set(2)));
		let mut pushed = all_run_types();
		while let Some(expected) = pushed.next() {
			assert_eq!(Some(expected), rb.pull());
		}

		assert_eq!(None, rb.pull());
	}

	fn all_run_types() -> impl Iterator<Item = Run> {
		use std::iter;
		use crate::MAX_RUN_SIZE;

		const SET: fn(u8) -> Run = Run::Set;
		const CLEAR: fn(u8) -> Run = Run::Clear;

		let sets = iter::repeat(SET).take(MAX_RUN_SIZE as usize);
		let clears = iter::repeat(CLEAR).take(MAX_RUN_SIZE as usize);
		let set_counts = 1u8..=MAX_RUN_SIZE;
		let clear_counts = set_counts.clone();

		fn make(pair: (fn(u8) -> Run, u8)) -> Run {
			pair.0(pair.1)
		}

		let sets = sets.zip(set_counts).map(make);
		let clears = clears.zip(clear_counts).map(make);
		sets.chain(clears)
	}
}
