use std::collections::HashMap;
use std::str::FromStr;
use crate::Day;

pub struct Day14 {
    pub input: Vec<String>,
}

#[derive(Debug)]
enum Tile {
    Rock,
    SandAtRest
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Coord {
    x: i32,
    y: i32
}

#[derive(Debug)]
struct CoordList {
    coords: Vec<Coord>
}

struct Cave {
    structure: HashMap<Coord, Tile>,
    lowest_y: i32
}

impl FromStr for CoordList {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let coordstrings: Vec<&str> = s.split(" -> ").collect();
        let mut coords = Vec::with_capacity(coordstrings.len());

        for coord in coordstrings {
            let spl: Vec<&str> = coord.split(",").collect();
            coords.push(Coord {
                x: spl[0].parse().unwrap(),
                y: spl[1].parse().unwrap()
            })
        }

        Ok(CoordList{ coords })
    }
}

impl CoordList {
    fn into_rock_coords(mut self) -> (HashMap<Coord, Tile>, i32) {
        if self.coords.is_empty() { return (HashMap::new(), 0); }

        let mut rocks = HashMap::new();
        let mut first = self.coords.pop().unwrap();

        // 'lowest' y
        let mut highesty = 0;

        while ! self.coords.is_empty() {
            let second = self.coords.pop().unwrap();

            let hx = first.x.max(second.x);
            let lx = first.x.min(second.x);

            let hy = first.y.max(second.y);
            let ly = first.y.min(second.y);

            highesty = highesty.max(hy);

            for x in lx..=hx {
                for y in ly..=hy {
                    rocks.insert(Coord { x, y }, Tile::Rock);
                }
            }

            first = second;
        }

        (rocks, highesty)
    }
}

impl Cave {
    fn create(input: &[String]) -> Self {
        let mut structure = HashMap::new();

        // 'lowest' y
        let mut lowest_y = 0;

        for line in input {
            let coords: CoordList = line.parse().unwrap();
            let (rocks, hy) = coords.into_rock_coords();

            lowest_y = lowest_y.max(hy);

            structure.extend(rocks);
        }

        Cave {
            structure,
            lowest_y
        }
    }

    fn add_floor(&mut self) {
        // new y height is also what we need to add left & right to support full pyramid
        // (x-yheight-1)..=(x+yheight+1)

        self.lowest_y += 2;

        for x in (500-self.lowest_y-1)..=(500+self.lowest_y+1) {
            self.structure.insert(Coord {
                x,
                y: self.lowest_y
            }, Tile::Rock);
        }
    }

    fn fall_one_sand(&mut self) -> bool {
        let mut cur = Coord { x: 500, y: 0};

        if let Some(Tile::SandAtRest) = self.structure.get(&cur) {
            // sand is already at rest at (500,0)
            return false;
        }

        let mutations = [
            |c: &mut Coord| c.y += 1,
            |c: &mut Coord| c.x -= 1,
            |c: &mut Coord| c.x += 2,
        ];
        loop {
            let mut movement = false;
            for f in mutations {
                f(&mut cur);
                if self.structure.get(&cur).is_none() {
                    movement = true;
                    break;
                }
            }
            if ! movement {
                self.structure.insert(Coord { x: cur.x -1, y: cur.y -1}, Tile::SandAtRest);
                return true;
            }
            if cur.y > self.lowest_y {
                // sand falls of the edge
                return false;
            }
        }
    }
}

impl Day for Day14 {
    fn part1(&self) -> String {
        let mut cave = Cave::create(&self.input);

        let mut num_sands = 0;
        while cave.fall_one_sand() {
            num_sands += 1;
        }

        num_sands.to_string()
    }

    fn part2(&self) -> String {
        let mut cave = Cave::create(&self.input);

        cave.add_floor();

        let mut num_sands = 0;
        while cave.fall_one_sand() {
            num_sands += 1;
        }

        num_sands.to_string()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT: &str = "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9";

    fn get_day(input_num: u8) -> Day14 {
        let inp = match input_num {
            0 => include_str!("../inputs/day14.txt"),
            1 => INPUT,
            _ => panic!(),
        };

        Day14 {
            input: inp.lines().map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn part1_example() {
        let d = get_day(1);

        assert_eq!("24", d.part1());
    }

    #[test]
    fn part2_example() {
        let d = get_day(1);

        assert_eq!("93", d.part2());
    }

    #[test]
    fn part1() {
        let d = get_day(0);

        assert_eq!("885", d.part1());
    }

    #[test]
    fn part2() {
        let d = get_day(0);

        assert_eq!("28691", d.part2());
    }
}
