//! Handles encoding of a Bitterlemon image

#[derive(Debug, Copy, Clone)]
pub enum Rle {
	SetRun(u8),
	ClearRun(u8),
	Frame,
}

#[derive(Debug)]
pub struct RleState<S> {
	state: Option<Rle>,
	source: S,
}

fn rle_new_run(pp: bool) -> Rle {
	if pp {Rle::SetRun(1)} else {Rle::ClearRun(1)}
}

const RLE_MAX_RUN: u8 = 64;

impl<S> RleState<S> {

	pub fn from_pixels(source: S) -> RleState<S> {
		RleState {
			state: None,
			source: source
		}
	}
}

impl<S: Iterator<Item=bool>> Iterator for RleState<S> {
	type Item = Rle;

	fn next(&mut self) -> Option<Rle> {
		use self::Rle::*;
		loop {

			let pp = match self.source.next() {
				Some(b) => b,
				None    => return match self.state {
					Some(x) => {
						let old = x;
						self.state = None;
						Some(old)
					},
					None => None
				}
			};

			match self.state {

				Some(SetRun(RLE_MAX_RUN)) => {
					self.state = Some(rle_new_run(pp));
					return Some(SetRun(RLE_MAX_RUN));
				},

				Some(ClearRun(RLE_MAX_RUN)) => {
					self.state = Some(rle_new_run(pp));
					return Some(ClearRun(RLE_MAX_RUN));
				}

				Some(SetRun(count)) if pp => {
					self.state = Some(SetRun(count + 1));
					continue;
				},

				Some(SetRun(count)) if ! pp => {
					let old = self.state;
					self.state = Some(ClearRun(1));
					return old;
				},

				Some(ClearRun(count)) if ! pp => {
					self.state = Some(ClearRun(count + 1));
					continue;
				},

				Some(ClearRun(count)) if pp => {
					let old = self.state;
					self.state = Some(SetRun(1));
					return old;
				},

				Some(Frame) => {
					unimplemented!();
				}

				Some(_) => {
					unreachable!();
				}

				None => {
					self.state = Some(rle_new_run(pp));
					continue;
				}
			}

		}
		unreachable!();
	}
}
