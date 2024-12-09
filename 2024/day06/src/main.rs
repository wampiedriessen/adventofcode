use std::cmp::PartialEq;
use std::collections::HashSet;
use std::fmt::{Debug, Formatter, Write};
use std::io;
use std::io::Read;
use std::str::FromStr;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum Direction {
    North,
    East,
    South,
    West,
}

impl Direction {
    fn next_pos(&self, pos: (i32, i32)) -> (i32, i32) {
        match self {
            Direction::North => (pos.0 - 1, pos.1),
            Direction::East => (pos.0, pos.1 + 1),
            Direction::South => (pos.0 + 1, pos.1),
            Direction::West => (pos.0, pos.1 - 1),
        }
    }

    fn turn(&self) -> Direction {
        match self {
            Direction::North => Direction::East,
            Direction::East => Direction::South,
            Direction::South => Direction::West,
            Direction::West => Direction::North,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum MapItem {
    Guard(Direction),
    Obstacle,
    Empty,
}

impl From<char> for MapItem {
    fn from(value: char) -> Self {
        match value {
            '#' => MapItem::Obstacle,
            '.' => MapItem::Empty,
            '^' => MapItem::Guard(Direction::North),
            '>' => MapItem::Guard(Direction::East),
            'v' => MapItem::Guard(Direction::South),
            '<' => MapItem::Guard(Direction::West),
            x => panic!("unknown mapitem {}", x),
        }
    }
}

#[derive(Clone)]
struct PuzzleMap {
    map: Vec<Vec<MapItem>>,
    guard_pos: (i32, i32),
}

impl Debug for PuzzleMap {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for row in &self.map {
            for col in row {
                f.write_char(match col {
                    MapItem::Empty => '.',
                    MapItem::Obstacle => '#',
                    MapItem::Guard(Direction::North) => '^',
                    MapItem::Guard(Direction::East) => '>',
                    MapItem::Guard(Direction::South) => 'v',
                    MapItem::Guard(Direction::West) => '<',
                })?
            }
            f.write_char('\n')?
        }
        Ok(())
    }
}

impl FromStr for PuzzleMap {
    type Err = ();

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let mut map = Self {
            map: vec![],
            guard_pos: (0, 0),
        };

        let mut y = 0;
        for line in input.lines() {
            let mut lineitems = Vec::new();

            let mut x = 0;
            for c in line.chars() {
                let item = MapItem::from(c);
                match &item {
                    MapItem::Guard(_) => {
                        map.guard_pos = (y, x);
                    }
                    _ => {}
                };
                lineitems.push(item);
                x += 1;
            }
            y += 1;
            map.map.push(lineitems);
        }

        Ok(map)
    }
}

impl PuzzleMap {
    fn is_outside(&self, pos: (i32, i32)) -> bool {
        pos.0 < 0 || pos.1 < 0
            || pos.0 >= self.map.len() as i32
            || pos.1 >= self.map[0].len() as i32
    }
    fn get_mapitem(&mut self, pos: (i32, i32)) -> &mut MapItem {
        self.map.get_mut(pos.0 as usize).unwrap()
            .get_mut(pos.1 as usize).unwrap()
    }
    fn next_step(&mut self) -> bool {
        let curpositem = self.get_mapitem(self.guard_pos).clone();
        match curpositem {
            MapItem::Guard(direction) => {
                let newpos = direction.next_pos(self.guard_pos);
                if self.is_outside(newpos) {
                    false
                } else {
                    let newpositem = self.get_mapitem(newpos);
                    match newpositem {
                        MapItem::Guard(_) => panic!("multiple guards"),
                        MapItem::Obstacle => {
                            *self.get_mapitem(self.guard_pos) = MapItem::Guard(direction.turn())
                        }
                        MapItem::Empty => {
                            *newpositem = MapItem::Guard(direction);
                            *self.get_mapitem(self.guard_pos) = MapItem::Empty;
                            self.guard_pos = newpos
                        }
                    };
                    true
                }
            }
            _ => panic!("no guard at curpos!"),
        }
    }
}

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    let mut positions_seen = HashSet::new();
    let mut possible_barriers = HashSet::new();

    let mut puzzlemap = PuzzleMap::from_str(&input).unwrap();
    loop {
        // part2
        let curdir = match *puzzlemap.get_mapitem(puzzlemap.guard_pos) {
            MapItem::Guard(direction) => direction,
            _ => panic!("no guard here")
        };
        let nextpos = curdir.next_pos(puzzlemap.guard_pos);
        if !puzzlemap.is_outside(nextpos)
            && *puzzlemap.get_mapitem(nextpos) == MapItem::Empty
            && !positions_seen.contains(&nextpos) {
            let mut new_map = puzzlemap.clone();
            *new_map.get_mapitem(puzzlemap.guard_pos) = MapItem::Guard(curdir.turn());
            *new_map.get_mapitem(nextpos) = MapItem::Obstacle;

            let mut found_loop = true;
            for _ in 0..10000 {
                if !new_map.next_step() {
                    found_loop = false;
                    break;
                }
            }

            if found_loop {
                possible_barriers.insert(nextpos);
            }
        }

        // part1
        positions_seen.insert(puzzlemap.guard_pos);
        if !puzzlemap.next_step() {
            break;
        }
    }

    println!("Part 1: {}", positions_seen.len());

    println!("Part 2: {}", possible_barriers.len());

    Ok(())
}
