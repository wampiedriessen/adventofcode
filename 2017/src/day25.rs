enum State {
	A,
	B,
	C,
	D,
	E,
	F,
}

pub fn part1() -> i32 {
	let mut state = State::A;
	let mut place:i64 = 8192;
	let mut tape:Vec<bool> = vec![false; 16384];
	for _ in 0..12399302 {
		let index:usize = place as usize;
		match state {
			State::A => {
				if tape[index] == false {
					tape[index] = true;
					place += 1;
					state = State::B;
				} else if tape[index] == true {
					tape[index] = false;
					place += 1;
					state = State::C;
				}
			},
			State::B => {
				if tape[index] == false {
					tape[index] = false;
					place -= 1;
					state = State::A;
				} else if tape[index] == true {
					tape[index] = false;
					place += 1;
					state = State::D;
				}
			},
			State::C => {
				if tape[index] == false {
					tape[index] = true;
					place += 1;
					state = State::D;
				} else if tape[index] == true {
					tape[index] = true;
					place += 1;
					state = State::A;
				}
			},
			State::D => {
				if tape[index] == false {
					tape[index] = true;
					place -= 1;
					state = State::E;
				} else if tape[index] == true {
					tape[index] = false;
					place -= 1;
					state = State::D;
				}
			},
			State::E => {
				if tape[index] == false {
					tape[index] = true;
					place += 1;
					state = State::F;
				} else if tape[index] == true {
					tape[index] = true;
					place -= 1;
					state = State::B;
				}
			},
			State::F => {
				if tape[index] == false {
					tape[index] = true;
					place += 1;
					state = State::A;
				} else if tape[index] == true {
					tape[index] = true;
					place += 1;
					state = State::E;
				}
			}
		}
		if place == -1 {
			tape.insert(0, false);
			place = 0;
		}
		if place as usize == tape.len() {
			tape.push(false);
			place = 0;
		}
	}

	let mut sum = 0;
	for p in tape {
		if p {
			sum += 1;
		}
	}
	sum
}

pub fn part2() -> i32 {
	0
}
