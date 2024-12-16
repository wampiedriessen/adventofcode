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

fn walk(best_scores: &mut HashMap<Pos, usize>, grid: &Grid, startpos: Pos) {
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
    walk(&mut best_scores, &grid, startpos);

    println!("Part 1: {}", best_score_at_yx(&best_scores, endpos));

    let best_path_tiles = calc_best_path_tiles(&best_scores, &grid, endpos[0]);
    println!("{:?}", best_path_tiles);
    println!("Part 2: {}", best_path_tiles.len());

    Ok(())
}

fn calc_best_path_tiles(best_scores: &HashMap<Pos, usize>, grid: &Grid, curpos: Pos) -> HashSet<Pos> {
    let curscore = best_score_at_yx(best_scores, get_all_dirs_at(curpos.0, curpos.1));

    let mut tiles = HashSet::new();
    tiles.insert((curpos.0, curpos.1, 0));
    let mut newposs: Vec<Pos> = Vec::new();
    for nextpos in [
            forward_step(grid, (curpos.0, curpos.1, 0)),
            forward_step(grid, (curpos.0, curpos.1, 1)),
            forward_step(grid, (curpos.0, curpos.1, 2)),
            forward_step(grid, (curpos.0, curpos.1, 3)),
        ] {
        if nextpos.is_none() { continue; }
        let nextpos = nextpos.unwrap();
        let actualpos = (nextpos.0, nextpos.1, (nextpos.2 + 2) % 4);
        if !best_scores.contains_key(&actualpos) { continue; }
        let score = best_scores[&actualpos];
        if score < curscore {
            newposs.push(actualpos);
        }
    }

    for pos in &newposs {
        tiles.insert((pos.0, pos.1, 0));
        for result in calc_best_path_tiles(best_scores, grid, *pos) {
            tiles.insert(result);
        }
    }

    tiles
}