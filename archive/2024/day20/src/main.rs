use std::collections::{HashMap, HashSet};
use std::io;
use std::io::Read;

const WALL: char = '#';
const START: char = 'S';
const END: char = 'E';

type Pos = (usize, usize);
type Grid = Vec<Vec<char>>;

fn forward_step(grid: &Grid, pos: Pos, cheats_left: i32) -> HashSet<Pos> {
    let mut states = HashSet::new();

    let range = 1.max(cheats_left);
    for dy in -range..=range {
        if dy < 0 && dy.abs() > pos.0 as i32 {
            continue;
        }
        if dy > 0 && dy as usize + pos.0 >= grid.len() {
            continue;
        }

        let xrange = range-(dy.abs());
        for dx in -xrange..=xrange {
            if dx == 0 && dy == 0 || dx.abs() + dy.abs() > range {
                continue;
            }
            if dx < 0 && dx.abs() > pos.1 as i32 {
                continue;
            }
            if dx > 0 && dx as usize + pos.1 >= grid[0].len() {
                continue;
            }

            let ny = pos.0 as i32 + dy;
            let nx = pos.1 as i32 + dx;

            let newpos = (ny as usize, nx as usize);
            if grid[newpos.0][newpos.1] == WALL {
                continue;
            }

            states.insert((ny as usize, nx as usize));
        }
    }

    states
}

fn no_cheat_flood(grid: &Grid, pos: Pos, step: usize, positions: &mut HashMap<Pos, usize>) {
    positions.insert(pos, step);
    for newpos in forward_step(grid, pos, 0) {
        if positions.contains_key(&newpos) { continue; }

        no_cheat_flood(grid, newpos, step + 1, positions);
    }
}

fn find_better_cheats(grid: &Grid, distance_from_start: &HashMap<Pos, usize>, distance_from_end: &HashMap<Pos, usize>, cheats: i32, margin: usize, limit: usize) -> usize {
    let mut better_scores = Vec::new();

    for cury in 0..grid.len() {
        for curx in 0..grid[0].len() {
            if grid[cury][curx] == WALL { continue; }

            for (ny, nx) in forward_step(grid, (cury, curx), cheats) {
                let delta = cury.abs_diff(ny) + curx.abs_diff(nx);

                let total = delta + distance_from_start[&(cury, curx)] + distance_from_end[&(ny, nx)];
                if total + margin < limit {
                    better_scores.push(total);
                }
            }
        }
    }

    better_scores.len()
}

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    let mut grid = Vec::new();
    let mut startpos = (0, 0);
    let mut endpos = (0, 0);

    for (y, line) in input.lines().enumerate() {
        let mut gridline = Vec::new();
        for (x, c) in line.chars().enumerate() {
            if c == START {
                startpos = (y, x);
            }
            if c == END {
                endpos = (y, x);
            }

            gridline.push(c);
        }
        grid.push(gridline);
    }

    let mut distance_from_end = HashMap::new();
    no_cheat_flood(&grid, endpos, 0, &mut distance_from_end);

    let mut distance_from_start = HashMap::new();
    no_cheat_flood(&grid, startpos, 0, &mut distance_from_start);

    let base_score = distance_from_end[&startpos];

    let p1 = find_better_cheats(&grid, &distance_from_start, &distance_from_end, 2, 99, base_score);
    println!("Part 1: {}", p1);

    let p2 = find_better_cheats(&grid, &distance_from_start, &distance_from_end, 20, 99, base_score);
    println!("Part 2: {}", p2);

    Ok(())
}
