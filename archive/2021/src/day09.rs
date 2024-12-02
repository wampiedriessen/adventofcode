use std::collections::{HashMap, HashSet};

use crate::Day;

pub struct Day09 {
    pub input: Vec<String>,
}

#[derive(Hash, Eq, PartialEq, Clone)]
struct Point {
    x: usize,
    y: usize
}

struct Map {
    width: usize,
    height: usize,
    map: HashMap<Point, usize>
}

impl Map {
    fn new(input: &Vec<String>) -> Self {
        let width = input[0].len();
        let height = input.len();

        let mut map = HashMap::new();

        let mut y = 0;
        for line in input {
            let mut x = 0;
            for c in line.split("") {
                if c == "" { continue; }
                map.insert(Point {x, y}, c.parse::<usize>().unwrap());
                x += 1;
            }
            y += 1;
        }

        Map {
            width,
            height,
            map
        }
    }

    fn is_min<I>(&self, p: usize, mut v: I) -> bool
        where I: Iterator<Item = usize>
    {
        v.all(|h| p < h)
    }

    fn get_adjacent(&self, p: &Point) -> Vec<Point> {
        let mw = self.width - 1;
        let mh = self.height - 1;
        match p {
            // left top corner
            Point { x, y } if *y ==0 && *x == 0 => vec![Point { x: 1, y: 0}, Point { x: 0, y: 1} ],
            // top right corner
            Point { x, y } if *y == 0 && *x == mw => vec![Point { x: mw - 1, y: 0}, Point { x: mw, y: 1} ],
            // bottom left
            Point { x, y } if *y == mh && *x == 0 => vec![Point { x: 0, y: mh - 1}, Point { x: 1, y: mh} ],
            // bottom right corner
            Point { x, y } if *y == mh && *x == mw => vec![Point { x: mw-1, y: mh}, Point { x: mw, y: mh-1} ],

            // left border
            Point { x, y } if *x == 0 => vec![Point { x: 1, y: *y}, Point { x: 0, y: y-1 }, Point { x: 0, y: y+1 } ],
            // right border
            Point { x, y } if *x == mw => vec![Point { x: mw-1, y: *y}, Point { x: mw, y: y-1 }, Point { x: mw, y: y+1 } ],
            // top border
            Point { x, y } if *y == 0 => vec![Point { x: *x, y: 1}, Point { x: x-1, y: 0 }, Point { x: x+1, y: 0 } ],
            // bottom border
            Point { x, y } if *y == mh => vec![Point { x: *x, y: mh-1}, Point { x: x-1, y: mh }, Point { x: x+1, y: mh } ],

            // all else
            Point { x, y } => vec![Point { x: *x, y: y+1}, Point { x: *x, y: y-1}, Point { x: x+1, y: *y}, Point { x: x-1, y: *y} ]
        }
    }

    fn is_low_point(&self, p: &Point) -> bool {
        let v = self.map[p];

        self.is_min(v, self.get_adjacent(p).iter().map(|pa| self.map[pa]))
    }

    fn get_basin_size(&self, p: &Point) -> usize {
        // We assume method is only called on low points
        // p = 1
        let mut seen = HashSet::new();
        seen.insert(p.clone());
        let mut todo: Vec<Point> = self.get_adjacent(p).into_iter().filter(|p| self.map[p] != 9).collect();

        while !todo.is_empty() {
            let np = todo.pop().unwrap();
            if seen.contains(&np) { continue; }

            let npval = self.map[&np];
            let newpoints = self.get_adjacent(&np).into_iter();

            seen.insert(np);

            let f = |new: &Point| !seen.contains(new) && self.map[new] > npval && self.map[new] != 9;

            for newpoint in newpoints.filter(f) {
                todo.push(newpoint);
            }
        }

        seen.len()
    }

    fn get_low_points(&self) -> Vec<Point> {
        let mut lowpoints = Vec::new();

        for x in 0..self.width {
            for y in 0..self.height {
                let p = Point { x, y };
                if self.is_low_point(&p) {
                    lowpoints.push(p);
                }
            }
        }

        lowpoints
    }


    fn get_low_points_val(&self) -> Vec<usize> {
        self.get_low_points()
            .iter()
            .map(|p| self.map[p])
            .collect()
    }
}

impl Day for Day09 {
    // naive implementation.. Didn't cut it for part 2
    fn part1(&self) -> String {
        let map = Map::new(&self.input);

        map
            .get_low_points_val()
            .iter()
            .map(|p| p + 1)
            .sum::<usize>()
            .to_string()
    }

    fn part2(&self) -> String {
        let map = Map::new(&self.input);

        let lowpoints = map.get_low_points();

        let mut basins = Vec::new();

        for lp in lowpoints {
            basins.push(map.get_basin_size(&lp));
        }

        basins.sort();
        basins.reverse();

        let mut s = 1;

        for basinsize in basins.iter().take(3) {
            s *= basinsize;
        }

        s.to_string()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT: &str = r#"2199943210
3987894921
9856789892
8767896789
9899965678"#;

    fn get_day(input_num: u8) -> Day09 {
        let inp = match input_num {
            0 => include_str!("../inputs/day09.txt"),
            1 => INPUT,
            _ => panic!(),
        };

        Day09 {
            input: inp.split('\n').map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn part1_example() {
        let d = get_day(1);

        assert_eq!("15", d.part1());
    }

    #[test]
    fn part2_example() {
        let d = get_day(1);

        assert_eq!("1134", d.part2());
    }

    #[test]
    fn part1() {
        let d = get_day(0);

        assert_eq!("588", d.part1());
    }

    #[test]
    fn part2() {
        let d = get_day(0);

        assert_eq!("964712", d.part2());
    }
}
