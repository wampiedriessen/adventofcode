use std::collections::{HashMap, HashSet};
use crate::Day;

pub struct Day12 {
    pub input: Vec<String>,
}

#[derive(Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
struct Coord {
    x: i32,
    y: i32,
}

impl Day12 {
    fn create_heightmap(&self) -> (Coord, HashMap<Coord, u8>, Coord) {
        let mut map: HashMap<Coord, u8> = HashMap::new();
        let mut start = Coord {x: 0, y: 0};
        let mut end = Coord {x: 0, y: 0};

        for (y, line) in self.input.iter().enumerate() {
            for (x, c) in line.chars().enumerate() {
                let coord = Coord{ x: x as i32, y: y as i32};
                let height = match c {
                    'S' => { start = coord; b'a'},
                    'E' => { end = coord; b'z'},
                    x => x as u8
                };

                map.insert(coord, height);
            }
        }

        (start, map, end)
    }
}

fn walk<T>(heightmap: &HashMap<Coord, u8>, highest_point: Coord, accept: T) -> i32
    where T: Fn(&Coord) -> bool
{
    let mut been = HashSet::new();
    let mut steps_away_map = HashMap::new();
    steps_away_map.insert(0, HashSet::from([highest_point]));

    let mut stepsaway = 0;
    loop {

        let cur: Coord;
        {
            let platform = steps_away_map.get_mut(&stepsaway).unwrap();
            if platform.is_empty() {
                stepsaway += 1;
                continue;
            }
            cur = platform.iter().next().cloned().unwrap();
            platform.remove(&cur);
            been.insert(cur);
        }
        let nextplatform = steps_away_map.entry(stepsaway + 1).or_default();

        if accept(&cur) { return stepsaway; }

        let curheight = heightmap[&cur];

        let directions = vec![
            Coord {x: cur.x + 1, y: cur.y },
            Coord {x: cur.x, y: cur.y + 1 },
            Coord {x: cur.x - 1, y: cur.y },
            Coord {x: cur.x, y: cur.y - 1 },
        ];

        for newcoord in directions {
            if let Some(newheight) = heightmap.get(&newcoord) {
                if curheight <= (*newheight + 1)  && !been.contains(&newcoord) {
                    nextplatform.insert(newcoord);
                }
            }
        }
    }
}

impl Day for Day12 {
    fn part1(&self) -> String {
        let (start, heightmap, end) = self.create_heightmap();

        walk(&heightmap, end, |cur| start == *cur).to_string()
    }

    fn part2(&self) -> String {
        let (_, heightmap, end) = self.create_heightmap();

        walk(&heightmap, end, |cur| b'a' == heightmap[cur]).to_string()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT: &str = "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi";

    fn get_day(input_num: u8) -> Day12 {
        let inp = match input_num {
            0 => include_str!("../inputs/day12.txt"),
            1 => INPUT,
            _ => panic!(),
        };

        Day12 {
            input: inp.lines().map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn part1_example() {
        let d = get_day(1);

        assert_eq!("31", d.part1());
    }

    #[test]
    fn part2_example() {
        let d = get_day(1);

        assert_eq!("29", d.part2());
    }

    #[test]
    fn part1() {
        let d = get_day(0);

        assert_eq!("425", d.part1());
    }

    #[test]
    fn part2() {
        let d = get_day(0);

        assert_eq!("418", d.part2());
    }
}
