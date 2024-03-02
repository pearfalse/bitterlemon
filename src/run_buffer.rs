//! Basic ring buffer to hold runs for a potential frame.

use core::{
	mem::MaybeUninit,
	ptr,
};

use crate::{
	Run,
};

use std::fmt;

/// Holds runs with a rudimentary deque-style interface.
///
/// Runs can queue up when frequently-changing bits are inserted into the encoder state,
/// which is too busy outputting a frame to ingest them.
#[derive(Clone)]
pub(crate) struct RunBuffer {
	store: [MaybeUninit<Run>; Self::cap_u()],
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
			// SAFETY: this is a type hack that projects the uninit state to the array elements
			// (see also: unstable stdlib feature `maybe_uninit_uninit_array`)
			MaybeUninit::<[MaybeUninit<Run>; Self::cap_u()]>::uninit().assume_init()
		};

		RunBuffer {
			store,
			head: 0,
			tail: 0,
			len: 0,
		}
	}

	// This is set from a combination of the bitterlemon algorithm and the current implementation;
	// the number comes from
	// - 16 bytes for a full frame
	// - + 1 byte frame header
	// - + 1 extra run in flight for that call
	pub const fn capacity() -> u8 { 18 }

	const fn cap_u() -> usize { Self::capacity() as usize }

	#[inline]
	fn inc_ptr(ptr: u8) -> u8 {
		match ptr.wrapping_add(1) {
			n if n == Self::capacity() => 0,
			n => n,
		}
	}

	#[inline]
	fn dec_ptr(ptr: u8) -> u8 {
		match ptr {
			n if n == 0 => Self::capacity() - 1,
			n => n.wrapping_sub(1),
		}
	}

	#[cfg(test)]
	fn head(&self) -> Option<&Run> {
		(self.len > 0).then(move || unsafe {
			// SAFETY: self.head always points to the first element, and we are not empty
			self.get_unchecked(self.head)
		})
	}

	#[cfg(test)]
	fn tail(&self) -> Option<&Run> {
		(self.len > 0).then(move || unsafe {
			// SAFETY: we have at least 1 element, and tail is always correctly initialised
			self.get_unchecked(Self::dec_ptr(self.tail))
		})
	}

	#[inline]
	#[cfg(test)]
	unsafe fn get_unchecked(&self, idx: u8) -> &Run {
		unsafe {
			// SAFETY: caller must ensure that idx is in range
			&*self.store.get_unchecked(idx as usize).as_ptr()
		}
	}

	#[inline]
	unsafe fn get_unchecked_mut(&mut self, idx: u8) -> *mut Run {
		unsafe {
			// SAFETY: caller must ensure that idx is in range
			self.store.get_unchecked_mut(idx as usize).as_mut_ptr()
		}
	}

	pub fn push_back(&mut self, run: Run) -> Result<(), Run> {
		if self.len == Self::capacity() { return Err(run); }
		debug_assert!(self.len < Self::capacity());

		unsafe {
			// SAFETY: self.tail is in range, and the element will not be read from
			ptr::write(self.get_unchecked_mut(self.tail), run);
		}

		self.tail = Self::inc_ptr(self.tail);
		self.len += 1;

		Ok(())
	}

	pub fn push_front(&mut self, run: Run) -> Result<(), Run> {
		if self.len == Self::capacity() { return Err(run); }
		debug_assert!(self.len < Self::capacity());

		let new_head = Self::dec_ptr(self.head);
		unsafe {
			// checked in debug builds
			debug_assert!(new_head < Self::capacity());
			ptr::write(self.get_unchecked_mut(new_head), run);
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
			let target = self.get_unchecked_mut(self.head);
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
	fn test_loop_stability() {
		let mut rb = RunBuffer::new();
		for _ in 0..(RunBuffer::capacity() * 10) {
			rb.push_back(Run::Set(1)).unwrap();
			assert_eq!(Some(Run::Set(1)), rb.pop_front());
		}
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
