#[derive(Clone)]
struct Component {
	id:i32,
	conn1:i32,
	conn2:i32,
}

pub fn part1() -> i32 {
	let input = include_str!("../inputs/day24.txt");

	run1(parse_input(input))
}

pub fn part2() -> i32 {
	let input = include_str!("../inputs/day24.txt");

	run2(parse_input(input))
}

fn run1(mut components:Vec<Component>) -> i32 {
	find_max(&components, vec![], 0)
}

fn run2(components:Vec<Component>) -> i32 {
	let (len, max) = find_max_len(&components, vec![], 0, 0);
	max
}

fn find_max(components:&Vec<Component>, mut seen:Vec<i32>, start_num:i32) -> i32 {
	let mut max:Vec<i32> = Vec::new();

	for component in components.iter() {
		if !seen.contains(&component.id) && (component.conn1 == start_num || component.conn2 == start_num) {
			let val = component.conn1 + component.conn2;
			seen.insert(0, component.id);
			if component.conn1 == start_num {
				max.push(find_max(&components, seen.to_vec(), component.conn2) + val);
			} else if component.conn2 == start_num {
				max.push(find_max(&components, seen.to_vec(), component.conn1) + val);
			}
			seen.remove(0);
		}
	}

	if max.len() == 0 {
		return 0;
	}

	max.sort();
	max[max.len()-1]
}

fn find_max_len(components:&Vec<Component>, mut seen:Vec<i32>, start_num:i32, len:i32) -> (i32, i32) {
	let mut maxlen:Vec<(i32, i32)> = Vec::new();

	for component in components.iter() {
		if !seen.contains(&component.id) && (component.conn1 == start_num || component.conn2 == start_num) {
			let val = component.conn1 + component.conn2;
			seen.insert(0, component.id);
			if component.conn1 == start_num {
				let (len, max) = find_max_len(&components, seen.to_vec(), component.conn2, len+1);
				maxlen.push((len, max+val));
			} else if component.conn2 == start_num {
				let (len, max) = find_max_len(&components, seen.to_vec(), component.conn1, len+1);
				maxlen.push((len, max+val));
			}
			seen.remove(0);
		}
	}

	if maxlen.len() == 0 {
		return (len, 0);
	}

	maxlen.sort_by(|&(_, ref a), &(_, ref b)| a.cmp(&b));
	maxlen.sort_by(|&(ref a, _), &(ref b, _)| a.cmp(&b));
	maxlen[maxlen.len()-1]
}

fn parse_input(input:&str) -> Vec<Component> {
	let lines:Vec<&str> = input.split_whitespace().collect();
	let mut out:Vec<Component> = Vec::new();

	let mut i = 0;
	for line in lines {
		let split:Vec<&str> = line.split("/").collect();

		out.push(Component {
			id: i,
			conn1: split[0].parse().unwrap(),
			conn2: split[1].parse().unwrap(),
		});
		i += 1;
	}
	out
}