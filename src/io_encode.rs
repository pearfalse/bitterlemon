//! Handles encoding of a Bitterlemon data stream.
// I/O variant, 0.3.x

use crate::{
	encode::Run,
	run_buffer::RunBuffer,
	MAX_RUN_SIZE,
	MAX_FRAME_SIZE,
};

#[derive(Debug)]
struct RunBuilder {
	run_buffer: RunBuffer,
}

impl RunBuilder {
	pub fn new() -> RunBuilder {
		RunBuilder {
			run_buffer: RunBuffer::new(),
		}
	}

	pub(crate) fn update(&mut self, bit: bool) -> Option<u8> {
		match self.run_buffer.tail_mut() {
			Some(tail) if tail.len() == MAX_RUN_SIZE || tail.bit() != bit => {
				debug_assert!({
					let l = self.run_buffer.len();
					l > 0 && l < MAX_FRAME_SIZE
				});
				// push new run
				self.run_buffer.push(Run::new(bit)).unwrap();
				// old run is guaranteed ready to dissipate
				Some(self.run_buffer.pull().unwrap().into())
			},
			Some(tail) => {
				debug_assert!(tail.bit() == bit);
				tail.increment();
				None
			},
			None => {
				// fresh run
				self.run_buffer.push(Run::new(bit)).unwrap();
				None
			},
		}
	}

	pub(crate) fn flush(self) -> Option<Run> {
		let mut this = self;
		this.run_buffer.pull()
	}
}

#[cfg(test)]
mod test_run_builder {
	use super::*;

	#[test]
	fn one_run() {
		let mut encoder = RunBuilder::new();
		const LEN: usize = 10;
		(0..LEN).for_each(|_| assert_eq!(None, encoder.update(true)));

		assert_eq!(Some(Run::Set(LEN as u8)), encoder.flush());
	}
}