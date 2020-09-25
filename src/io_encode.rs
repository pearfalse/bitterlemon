//! Handles encoding of a Bitterlemon data stream.
// I/O variant, 0.3.x

use crate::{
	encode::Run,
	MAX_RUN_SIZE,
	MAX_FRAME_SIZE,
};

use std::mem::replace;

#[derive(Debug)]
struct RunBuilder {
	current: Option<Run>,
}

impl RunBuilder {
	pub fn new() -> RunBuilder {
		RunBuilder {
			current: None,
		}
	}

	pub(crate) fn update(&mut self, bit: bool) -> Option<Run> {
		match self.current.as_mut() {
			Some(tail) if tail.len() < MAX_RUN_SIZE && tail.bit() == bit => {
				tail.increment();
				None
			},
			_ => {
				// fresh run
				replace(&mut self.current, Some(Run::new(bit)))
			},
		}
	}

	pub(crate) fn flush(self) -> Option<Run> {
		let mut this = self;
		this.current.take()
	}
}

#[cfg(test)]
mod test_run_builder {
	use super::*;

	#[test]
	fn one_run() {
		let mut encoder = RunBuilder::new();
		const LEN: u8 = 64;
		(0..LEN).for_each(|_| assert_eq!(None, encoder.update(true)));

		assert_eq!(Some(Run::Set(LEN)), encoder.flush());
	}

	#[test]
	fn two_runs() {
		let mut encoder = RunBuilder::new();
		const LEN: u8 = 96;
		(0..MAX_RUN_SIZE).for_each(|_| assert_eq!(None, encoder.update(true)));

		// 65th run
		assert_eq!(Some(Run::Set(MAX_RUN_SIZE).into()), encoder.update(true));

		(MAX_RUN_SIZE+1 .. LEN).for_each(|_| assert_eq!(None, encoder.update(true)));
		assert_eq!(Some(Run::Set(LEN - MAX_RUN_SIZE).into()), encoder.flush());
	}

	#[test]
	fn alternate() {
		let mut encoder = RunBuilder::new();

		assert_eq!(None, encoder.update(true));

		for _ in 0..100 {
			assert_eq!(Some(Run::Set(1)), encoder.update(false));
			assert_eq!(Some(Run::Clear(1)), encoder.update(true));
		}

		assert_eq!(Some(Run::Set(1)), encoder.flush());
	}
}