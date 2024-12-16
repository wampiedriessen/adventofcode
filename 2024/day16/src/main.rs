use std::collections::{HashMap, HashSet};
use std::io;
use std::io::Read;

const NORTH: i8 = 0;
const EAST: i8 = 1;
const SOUTH: i8 = 2;
const WEST: i8 = 3;

const WALL: char = '#';
const START: char = 'S';
const END: char = 'E';
// const EMPTY: char = '.';

type Pos = (usize, usize, i8);
type Grid = Vec<Vec<char>>;

fn walk(best_scores: &mut HashMap<Pos, usize>, grid: &Grid, startpos: Pos, routes: &mut HashMap<Pos, Vec<(usize, Pos)>>) {
    let mut stack = vec![(0, startpos)];

    while !stack.is_empty() {
        let (curscore, pos) = stack.pop().unwrap();

        if best_scores.contains_key(&pos) && curscore >= best_scores[&pos] { continue; }
        best_scores.insert(pos, curscore);

        if grid[pos.0][pos.1] == END {
            continue;
        }

        let mut next_poses = Vec::new();
        // forward
        if let Some(newpos) = forward_step(grid, pos) {
            next_poses.push(newpos);
        }
        // CW
        next_poses.push((pos.0, pos.1, (pos.2 + 1) % 4));
        // CCW
        next_poses.push((pos.0, pos.1, (pos.2 + 3) % 4));

        for newpos in next_poses {
            let point = if newpos.2 == pos.2 { 1 } else { 1000 };

            let e = routes.entry(newpos).or_insert_with(Vec::new);
            e.push((curscore, pos));
            stack.push((curscore + point, newpos));
        }
    }
}

fn forward_step(grid: &Grid, pos: Pos) -> Option<Pos> {
    let newpos = match pos.2 {
        NORTH if pos.0 > 0 => Some((pos.0 - 1, pos.1, pos.2)),
        EAST if pos.1 < grid[0].len() - 1 => Some((pos.0, pos.1 + 1, pos.2)),
        SOUTH if pos.0 < grid.len() - 1 => Some((pos.0 + 1, pos.1, pos.2)),
        WEST if pos.1 > 0 => Some((pos.0, pos.1 - 1, pos.2)),
        _ => None
    };

    if let Some(newpos) = newpos {
        if grid[newpos.0][newpos.1] != WALL {
            Some(newpos)
        } else {
            None
        }
    } else {
        None
    }
}

fn best_score_at_yx(best_scores: &HashMap<Pos, usize> , poslist: [Pos; 4]) -> usize {
    *poslist.into_iter().map(|p| best_scores.get(&p).unwrap_or(&usize::MAX)).min().unwrap()
}

fn get_all_dirs_at(y: usize, x: usize) -> [Pos; 4] {
    [(y, x, 0), (y, x, 1), (y, x, 2), (y, x, 3)]
}

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    let mut grid = Vec::new();
    let mut startpos = (0, 0, 1);
    let mut endpos = [(0, 0, 0); 4];

    for (y, line) in input.lines().enumerate() {
        let mut gridline = Vec::new();
        for (x, c) in line.chars().enumerate() {
            if c == START {
                startpos = (y, x, 1);
            }
            if c == END {
                // #beun
                endpos = get_all_dirs_at(y, x);
            }

            gridline.push(c);
        }
        grid.push(gridline);
    }

    let mut best_scores = HashMap::new();
    let mut routes = HashMap::new();
    walk(&mut best_scores, &grid, startpos, &mut routes);

    let best_score = best_score_at_yx(&best_scores, endpos);
    println!("Part 1: {}", best_score);

    let mut positions = HashSet::new();
    let mut stack = Vec::new();
    for pos in endpos {
        if best_scores.contains_key(&pos) && best_scores[&pos] == best_score {
            positions.insert((pos.0, pos.1, 0));
            stack.push(pos);
        }
    }

    let mut seen = HashSet::new();
    while !stack.is_empty() {
        let pos = stack.pop().unwrap();
        if seen.contains(&pos) { continue; }
        seen.insert(pos);

        for (score, p) in &routes[&pos] {
            if score < &best_scores[&pos] {
                positions.insert((p.0, p.1, 0));
                stack.push(*p);
            }
        }
    }
    println!("Part 2: {}", positions.len());

    Ok(())
}
