use std::ops::{Add, AddAssign};

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn part1_sample_test() {
		let example1 = "p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>
p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>";

		assert_eq!(0, run1(example1.to_string()));
	}

	#[test]
	fn part2_sample_test() {
		let example2 = "p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>
p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>
p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>
p=<3,0,0>, v=<-1,0,0>, a=<0,0,0>";

		assert_eq!(1, run2(example2.to_string()));
	}

	#[test]
	fn part1_test() {
		assert_eq!(91, part1());
	}

	#[test]
	fn part2_test() {
		assert_eq!(567, part2());
	}
}

#[derive(Debug, Copy, Clone, PartialEq)]
struct Vector {
	x:i32,
	y:i32,
	z:i32,
}

impl Vector {
	pub fn dist(&self) -> i32 {
		return self.x.abs() +
			self.y.abs() +
			self.z.abs();
	}
}

impl Add for Vector {
	type Output = Vector;

	fn add(self, other: Vector) -> Vector {
		Vector {
			x: self.x + other.x,
			y: self.y + other.y,
			z: self.z + other.z,
		}
	}
}

impl AddAssign for Vector {
	fn add_assign(&mut self, other: Vector) {
		*self = *self + other;
	}
}

#[derive(Debug, Copy, Clone)]
struct Particle {
	nr:u32,
	pos:Vector,
	vel:Vector,
	acc:Vector,
}

impl Particle {
	pub fn step(&mut self) {
		self.vel += self.acc;
		self.pos += self.vel;
	}
}

pub fn part1() -> u32 {
	let input = include_str!("../inputs/day20.txt");

	return run1(input.to_string());
}

pub fn part2() -> usize {
	let input = include_str!("../inputs/day20.txt");

	return run2(input.to_string());
}

fn run1(input:String) -> u32 {
	let mut particles:Vec<Particle> = parse_input(input);

	particles.sort_by(|a, b| a.pos.dist().cmp(&b.pos.dist()));
	particles.sort_by(|a, b| a.vel.dist().cmp(&b.vel.dist()));
	particles.sort_by(|a, b| a.acc.dist().cmp(&b.acc.dist()));

	return particles[0].nr;
}

fn run2(input:String) -> usize {
	let mut particles:Vec<Particle> = parse_input(input);

	let target = 200;
	let mut collission_free_loops = 0;
	let mut last_len = particles.len();

	loop {
		let mut seen:Vec<Vector> = Vec::new();
		let mut collisions:Vec<Vector> = Vec::new();
		
		for part in particles.iter() {
			if seen.contains(&part.pos) {
				collisions.push(part.pos);
			}
			seen.push(part.pos);
		}

		particles.retain(|p| !collisions.contains(&p.pos));

		for part in particles.iter_mut() {
			part.step();
		}

		collission_free_loops += 1;
		if last_len > particles.len() {
			last_len = particles.len();
			collission_free_loops = 0;
		}
		if collission_free_loops == target { break; }
	}

	return particles.len();
}

fn parse_input(input:String) -> Vec<Particle> {
	let mut particles:Vec<Particle> = Vec::new();

	let lines:Vec<&str> = input.split("\n").collect();
	let mut index = 0;

	for line in lines {
		particles.push(parse_line(line.to_string(), index).clone());
		index += 1;
	}

	return particles;
}

fn parse_line(line:String, nr:u32) -> Particle {
	let vects:Vec<&str> = line.split(", ").collect();
	return Particle {
		nr: nr,
		pos: parse_vector(vects[0].to_string()),
		vel: parse_vector(vects[1].to_string()),
		acc: parse_vector(vects[2].to_string()),
	}
}

fn parse_vector(vect:String) -> Vector {
	let numbers = &vect[3..(vect.len()-1)];
	let dimensions:Vec<&str> = numbers.split(",").collect();

	return Vector {
		x: dimensions[0].parse().unwrap(),
		y: dimensions[1].parse().unwrap(),
		z: dimensions[2].parse().unwrap(),
	}
}