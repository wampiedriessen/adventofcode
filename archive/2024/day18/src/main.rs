use std::collections::{BinaryHeap, HashSet};
use std::io;
use std::io::Read;

const BOUNDS: usize = 70;
const INITIAL_FALL: usize = 1024;

type Pos = (usize, usize);
#[derive(PartialEq, Eq, Debug)]
struct State(usize, Pos);

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.0.cmp(&self.0)
    }
}

fn next_poses(pos: &Pos) -> Vec<Pos> {
    let mut r = Vec::new();
    if pos.0 > 0 { r.push((pos.0 - 1, pos.1)); }
    if pos.1 > 0 { r.push((pos.0, pos.1 - 1)); }
    if pos.0 < BOUNDS { r.push((pos.0 + 1, pos.1)); }
    if pos.1 < BOUNDS { r.push((pos.0, pos.1 + 1)); }
    r
}

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    let mut falls_at: Vec<Pos> = Vec::new();

    for line in input.lines() {
        let (a, b) = line.split_once(',').unwrap();

        falls_at.push((a.parse().unwrap(), b.parse().unwrap()));
    }

    let mut walls: HashSet<Pos> = falls_at.iter().take(INITIAL_FALL).cloned().collect();
    let best_score = simulate(&walls);

    println!("Part 1: {}", best_score);

    // Fuck it, let's brute force it :P
    for i in 0..(falls_at.len() - INITIAL_FALL - 1) {
        let byte = falls_at[i + INITIAL_FALL];
        walls.insert(byte);

        if simulate(&walls) == usize::MAX {
            println!("Part 2: {},{}", byte.0, byte.1);
            break;
        }
    }

    Ok(())
}

fn simulate(walls: &HashSet<Pos>) -> usize {
    let mut seen = HashSet::new();
    let mut todo: BinaryHeap<State> = BinaryHeap::new();

    todo.push(State(0, (0, 0)));

    let mut best_score = usize::MAX;
    while let Some(state) = todo.pop() {
        let pos = state.1;

        if pos.1 == BOUNDS && pos.0 == BOUNDS {
            best_score = best_score.min(state.0);
            continue;
        }

        for newpos in next_poses(&pos) {
            if seen.contains(&newpos) { continue;}
            if walls.contains(&newpos) { continue; }
            seen.insert(newpos);
            todo.push(State(state.0+1, newpos));
        }
    }

    best_score
}