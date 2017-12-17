#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn part1_sample_test() {
    assert_eq!(5, run1(3, 10));
  }

  #[test]
  fn part2_sample_test() {
    assert_eq!(9, run2(3, 10));
  }

  #[test]
  fn part1_test() {
    assert_eq!(1025, part1());
  }

  #[test]
  fn part2_test() {
    assert_eq!(37803463, part2());
  }
}


pub fn part1() -> u32 {
	let input = include_str!("../inputs/day17.txt");

	return run1(input.parse::<u32>().unwrap(), 2018);
}

pub fn part2() -> u32{
	let input = include_str!("../inputs/day17.txt");

	return run2(input.parse::<u32>().unwrap(), 50000000);
}

fn run1(input:u32, iterations:u32) -> u32 {
	let mut buff:Vec<u32> = Vec::with_capacity(iterations as usize);
	buff.push(0);
	let mut index:u32 = 0;
	for i in 1..iterations {
		index = (index + input) % buff.len() as u32;

		index += 1;
		buff.insert(index as usize, i);
	}

	return buff[(index+1) as usize % buff.len()];
}

fn run2(input:u32, iterations:u32) -> u32 {
	let mut afterzero = 1;
	let mut index:u32 = 1;

	for i in 2..iterations {
		index = (index + input) % i as u32;
		index += 1;
		if index == 1 {
			afterzero = i;
		}
	}

	return afterzero;
}