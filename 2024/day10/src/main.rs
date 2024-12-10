use std::collections::{HashMap, HashSet};
use std::io;
use std::io::Read;

fn get_neighbours(pos: &(i32, i32), level: &HashSet<(i32, i32)>) -> HashSet<(i32, i32)> {
    [
        (pos.0 + 1, pos.1),
        (pos.0 - 1, pos.1),
        (pos.0, pos.1 + 1),
        (pos.0, pos.1 - 1),
    ].into_iter().filter(|l| level.contains(l)).collect::<HashSet<_>>()
}

fn trail_rating_1(pos: (i32, i32), levels: &HashMap<u8, HashSet<(i32, i32)>>, level: u8) -> HashSet<(i32, i32)> {
    let neighbours = get_neighbours(&pos, levels.get(&(level+1)).unwrap());
    if level == 8 {
        return neighbours;
    }
    let mut ret = HashSet::new();
    for n in neighbours {
       ret.extend(trail_rating_1(n, levels, level+1));
    }
    ret
}

fn trail_rating_2(pos: (i32, i32), levels: &HashMap<u8, HashSet<(i32, i32)>>, level: u8) -> usize {
    let neighbours = get_neighbours(&pos, levels.get(&(level+1)).unwrap());
    if level == 8 {
        return neighbours.len();
    }
    let mut sum = 0;
    for n in neighbours {
        sum += trail_rating_2(n, levels, level+1);
    }
    sum
}

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    let mut levels = HashMap::new();

    for (y, line) in input.trim().lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            levels.entry(c as u8 - b'0').or_insert(HashSet::new()).insert((x as i32, y as i32));
        }
    }

    let mut trail_scores_1 = 0;
    let mut trail_scores_2 = 0;
    for (x, y) in levels.get(&0).unwrap() {
        trail_scores_1 += trail_rating_1((*x, *y), &levels, 0).len();
        trail_scores_2 += trail_rating_2((*x, *y), &levels, 0);
    }

    println!("Part 1: {}", trail_scores_1);
    println!("Part 2: {}", trail_scores_2);

    Ok(())
}