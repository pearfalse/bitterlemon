//! Basic ring buffer to hold runs for a potential frame.

use std::{
	fmt,
	mem::MaybeUninit,
	ptr,
};

use crate::{
	encode::Run,
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

	pub const fn capacity() -> u8 { 18 }

	#[inline(always)]
	fn inc_ptr(ptr: u8) -> u8 {
		match ptr.wrapping_add(1) {
			n if n == Self::capacity() => 0,
			n => n,
		}
	}

	#[inline(always)]
	fn dec_ptr(ptr: u8) -> u8 {
		match ptr.wrapping_sub(1) {
			n if n == u8::max_value() => Self::capacity() - 1,
			n => n,
		}
	}

	pub fn len(&self) -> u8 { self.len }

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

	pub fn push_back(&mut self, run: Run) -> Result<(), Run> {
		if self.len == Self::capacity() { return Err(run); }

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

	pub fn push_front(&mut self, run: Run) -> Result<(), Run> {
		if self.len == Self::capacity() { return Err(run); }

		let new_head = Self::dec_ptr(self.head);
		unsafe {
			// checked in debug builds
			debug_assert!(new_head < Self::capacity());
			let target = self.store.get_unchecked_mut(new_head as usize).as_mut_ptr();
			ptr::write(target, run);
		}

		self.head = new_head;
		self.len += 1;

		Ok(())
	}

	pub fn pop_front(&mut self) -> Option<Run> {
		if self.len == 0 { return None; }

		let run = unsafe {
			// checked in debug builds
			debug_assert!(self.head < Self::capacity());
			let target = self.store.get_unchecked_mut(self.head as usize).as_ptr();
			// ptr::read isn't strictly necessary here, since Run is Copy and !Drop, but it
			// provides symmetry with `push_back`, and doesn't require the above invariants
			ptr::read(target)
		};

		self.len = self.len.wrapping_sub(1); // will not underflow; see top of method
		self.head = Self::inc_ptr(self.head);

		Some(run)
	}
}

impl Default for RunBuffer {
	fn default() -> RunBuffer {
		RunBuffer::new()
	}
}

#[cfg(test)]
mod test_run_buffer {
	use super::*;

	#[test]
	fn basic() {
		let mut rb = RunBuffer::new();
		assert_eq!(None, rb.pop_front());

		let runs = [
			Run::Set(1),
			Run::Set(2),
			Run::Clear(3),
			Run::Clear(4),
		];

		for run in runs.iter().copied() {
			rb.push_back(run).unwrap();
			assert_eq!(Some(&runs[0]), rb.head());
			assert_eq!(Some(&run), rb.tail());
		}

		let mut read_back = runs.iter().copied();
		while let expected @ Some(_) = read_back.next() {
			assert_eq!(expected, rb.pop_front());
		}

		assert_eq!(None, rb.pop_front()); // we emptied it
	}

	#[test]
	fn pop_front() {
		let mut rb = RunBuffer::new();
		rb.push_front(Run::Set(1)).unwrap();
		rb.push_front(Run::Set(2)).unwrap();

		assert_eq!(Some(Run::Set(2)), rb.pop_front());
		assert_eq!(Some(Run::Set(1)), rb.pop_front());
	}

	#[test]
	fn fill_aligned() {
		fill_impl(|_| {})
	}

	#[test]
	fn fill_unaligned() {
		fill_impl(|rb| {
			rb.push_back(Run::Clear(1)).unwrap();
			rb.push_back(Run::Clear(1)).unwrap();
			rb.push_back(Run::Clear(1)).unwrap();
			rb.pop_front();
			rb.pop_front();
			rb.pop_front();
		})
	}

	fn fill_impl(operation: fn(&mut RunBuffer)) {
		let mut rb = RunBuffer::new();
		operation(&mut rb);

		for run in fill_with_runs() {
			rb.push_back(run).unwrap();
		}

		assert_eq!(Err(Run::Set(2)), rb.push_back(Run::Set(2)));
		let mut pushed = fill_with_runs();
		while let Some(expected) = pushed.next() {
			assert_eq!(Some(expected), rb.pop_front());
		}

		assert_eq!(None, rb.pop_front());
	}

	fn fill_with_runs() -> impl Iterator<Item = Run> {
		std::iter::repeat(Run::Clear(2)).take(RunBuffer::capacity() as usize)
	}
}
