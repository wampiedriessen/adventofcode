use std::cmp::PartialEq;
use std::fmt::{Display, Formatter, Write};
use std::io;
use std::io::Read;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
enum Tile {
    Wall,
    Box,
    BoxLeft,
    BoxRight,
    Robot,
    Empty,
}

impl Tile {
    fn from_char(c: char) -> Tile {
        match c {
            '.' => Tile::Empty,
            '#' => Tile::Wall,
            '@' => Tile::Robot,
            'O' => Tile::Box,
            _ => panic!("Unknown tile: {}", c),
        }
    }
}

impl Display for Tile {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Tile::Wall => f.write_char('#'),
            Tile::Box => f.write_char('O'),
            Tile::Robot => f.write_char('@'),
            Tile::Empty => f.write_char('.'),
            Tile::BoxLeft => f.write_char('['),
            Tile::BoxRight => f.write_char(']'),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum Direction {
    North,
    South,
    West,
    East,
}

impl Direction {
    fn from_char(c: char) -> Direction {
        match c {
            'v' => Direction::South,
            '^' => Direction::North,
            '<' => Direction::West,
            '>' => Direction::East,
            _ => panic!("Unknown direction: {}", c),
        }
    }
}

fn print_grid(grid: &Vec<Vec<Tile>>) {
    for y in 0..grid.len() {
        for x in 0..grid[y].len() {
            print!("{}", grid[y][x]);
        }
        println!();
    }
}

fn do_moves(grid: &mut Vec<Vec<Tile>>, instructions: &Vec<Direction>) {
    let mut position = (0, 0);
    for y in 0..grid.len() {
        for x in 0..grid[y].len() {
            if grid[y][x] == Tile::Robot {
                position = (y, x);
            }
        }
    }

    for instruction in instructions {
        let mut tiles_to_move = calc_moveable_tiles(grid, &position, instruction);

        while let Some(pos) = tiles_to_move.pop() {
            let newpos = match instruction {
                Direction::North => (pos.0 - 1, pos.1),
                Direction::South => (pos.0 + 1, pos.1),
                Direction::East => (pos.0, pos.1 + 1),
                Direction::West => (pos.0, pos.1 - 1),
            };
            grid[newpos.0][newpos.1] = grid[pos.0][pos.1];
            grid[pos.0][pos.1] = Tile::Empty;
            position = newpos;
        }
    }
}

fn calc_moveable_tiles(
    grid: &mut Vec<Vec<Tile>>,
    position: &(usize, usize),
    instruction: &Direction,
) -> Vec<(usize, usize)> {
    let mut moveable_tiles: Vec<(usize, usize)> = Vec::new();
    let mut pos_queue = Vec::new();
    pos_queue.push(*position);

    while !pos_queue.is_empty() {
        let mut curpos = pos_queue.remove(0);

        while grid[curpos.0][curpos.1] != Tile::Empty {
            if !moveable_tiles.contains(&curpos) {
                moveable_tiles.push(curpos);
            }

            match instruction {
                Direction::North => { curpos.0 -= 1; },
                Direction::South => { curpos.0 += 1; },
                Direction::West => { curpos.1 -= 1; },
                Direction::East => { curpos.1 += 1; },
            }

            if grid[curpos.0][curpos.1] == Tile::Wall { return Vec::new(); }

            if *instruction == Direction::North || *instruction == Direction::South {
                if grid[curpos.0][curpos.1] == Tile::BoxLeft {
                    pos_queue.push((curpos.0, curpos.1 + 1));
                } else if grid[curpos.0][curpos.1] == Tile::BoxRight {
                    pos_queue.push((curpos.0, curpos.1 - 1));
                }
            }
        }
    }

    moveable_tiles
}

fn gps_sum(grid: &Vec<Vec<Tile>>) -> usize {
    let mut gps_sum = 0;
    for y in 0..grid.len() {
        for x in 0..grid[y].len() {
            if grid[y][x] == Tile::Box || grid[y][x] == Tile::BoxLeft {
                gps_sum += 100 * y + x;
            }
        }
    }

    gps_sum
}

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    let mut grid = Vec::new();
    let mut gridp2 = Vec::new();
    let mut instructions = Vec::new();
    let mut read_route = false;

    for line in input.lines() {
        if line.is_empty() {
            read_route = true;
            continue;
        }
        if read_route {
            instructions.extend(line.chars().map(|c| Direction::from_char(c)));
            continue;
        }
        let mut gridline = Vec::new();
        for c in line.chars() {
            gridline.push(Tile::from_char(c));
        }

        let mut gridline2 = Vec::new();
        for tile in &gridline {
            if *tile == Tile::Box {
                gridline2.extend([Tile::BoxLeft, Tile::BoxRight].iter());
            } else if *tile == Tile::Robot {
                gridline2.extend([Tile::Robot, Tile::Empty].iter());
            } else {
                gridline2.extend([*tile, *tile].iter());
            }
        }
        grid.push(gridline);
        gridp2.push(gridline2);
    }

    do_moves(&mut grid, &instructions);
    do_moves(&mut gridp2, &instructions);

    println!("Part 1: {}", gps_sum(&grid));
    println!("Part 2: {}", gps_sum(&gridp2));

    Ok(())
}
