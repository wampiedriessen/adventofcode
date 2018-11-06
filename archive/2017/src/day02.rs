#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn part1_sample_test() {
		assert_eq!(18, run1(vec!["5\t1\t9\t5","7\t5\t3","2\t4\t6\t8"]));
	}

    #[test]
	fn part2_sample_test() {
		assert_eq!(9, run2(vec!["5\t9\t2\t8","9\t4\t7\t3","3\t8\t6\t5"]));
	}

	#[test]
	fn part1_test() {
		assert_eq!(36766, part1());
	}

	#[test]
	fn part2_test() {
		assert_eq!(261, part2());
	}
}

pub fn part1() -> i32 {
	let input = include_str!("../inputs/day02.txt").trim();

	return run1(input.split("\n").collect());
}

pub fn part2() -> i32 {
	let input = include_str!("../inputs/day02.txt").trim();

	return run2(input.split("\n").collect());
}

fn run1(input:Vec<&str>) -> i32 {
	let mut sum: i32 = 0;
	let mut v = Vec::with_capacity(15);

	for line in input {
		for num in line.trim().split_whitespace() {
			let x: i32 = num.parse().expect("No INT! :(");
			v.push(x);
		}
		v.sort();

		sum += v[v.len()-1] - v[0];
		v.clear();
	}
	return sum;
}

fn run2(input:Vec<&str>) -> i32 {
	let mut sum: i32 = 0;
	let mut v = Vec::with_capacity(15);

	for line in input {
		for num in line.trim().split_whitespace() {
			let x: i32 = num.parse().expect("No INT! :(");
			v.push(x);
		}

		v.sort();

		sum += divisive_numbers(v.clone());
		v.clear();
	}
	return sum;
}

fn divisive_numbers(v: Vec<i32>) -> i32 {
	for i in 0..v.len()-1 {
		for j in i+1..v.len() {
			if v[i] % v[j] == 0 {
				return v[i] / v[j] as i32;
			}
			if v[j] % v[i] == 0 {
				return v[j] / v[i] as i32;
			}
		}
	}
	return 0;
}