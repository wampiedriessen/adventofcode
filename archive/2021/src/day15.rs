use std::cmp::Ordering;
use std::collections::BinaryHeap;
use std::collections::HashMap;

use crate::Day;

pub struct Day15 {
    pub input: Vec<String>,
}

#[derive(Copy, Clone, Eq, PartialEq)]
struct Node {
    score: u32,
    position: (u32, u32),
}

impl Ord for Node {
    fn cmp(&self, other: &Self) -> Ordering {
        other
            .score
            .cmp(&self.score)
            .then_with(|| self.position.cmp(&other.position))
    }
}

impl PartialOrd for Node {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

struct Grid {
    width: u32,
    height: u32,
    risk_levels: Vec<Vec<u32>>,
}

impl Grid {
    fn add_risk(mut risk: u32, add: u32) -> u32 {
        for _ in 0..add {
            if risk == 9 {
                risk = 1;
            } else {
                risk += 1;
            }
        }
        risk
    }

    fn new(input: &Vec<String>, multiply: u32) -> Self {
        let width = input[0].len() as u32;
        let height = input.len() as u32;

        let mut risk_levels = Vec::new();

        for addy in 0..multiply {
            for line in input {
                let chars: Vec<char> = line.chars().collect();
                let mut risk_line = Vec::new();
                for addx in 0..multiply {
                    for c in &chars {
                        risk_line.push(Grid::add_risk(c.to_digit(10).unwrap(), addy + addx));
                    }
                }
                risk_levels.push(risk_line);
            }
        }

        Grid {
            width: width * multiply,
            height: height * multiply,
            risk_levels,
        }
    }

    fn heur_score(&self, node: (u32, u32)) -> u32 {
        let (x, y) = node;

        self.width - 1 - x + self.height - 1 - y
    }

    fn calculate_return_path(&self, best_from: HashMap<(u32, u32), (u32, u32)>) -> Vec<(u32, u32)> {
        let mut cur = (self.width - 1, self.height - 1);
        let mut path_stack = vec![cur.clone()];

        while best_from.contains_key(&cur) {
            cur = best_from[&cur];
            path_stack.push(cur.clone());
        }

        path_stack
    }

    fn neighbours(&self, (x, y): (u32, u32)) -> Vec<(u32, u32)> {
        vec![
            (x.checked_sub(1), Some(y)),
            (Some(x), y.checked_sub(1)),
            (Some(x), y.checked_add(1)),
            (x.checked_add(1), Some(y)),
        ]
        .iter()
        .filter_map(|p| match p {
            (Some(x), Some(y)) if *x < self.width && *y < self.height => Some((*x, *y)),
            _ => None,
        })
        .collect()
    }

    fn a_star(&self) -> Vec<(u32, u32)> {
        let mut todo = BinaryHeap::new(); // priority queue, thanks to `impl Ord`
        let mut best_from = HashMap::new();
        let mut best_score = HashMap::from([((0, 0), 0)]);

        todo.push(Node {
            position: (0, 0),
            score: self.heur_score((0, 0)),
        });

        while let Some(Node { position, score: _ }) = todo.pop() {
            if position == (self.width - 1, self.height - 1) {
                return self.calculate_return_path(best_from);
            }

            for n @ (nx, ny) in self.neighbours(position) {
                let possible_score =
                    best_score[&position] + self.risk_levels[ny as usize][nx as usize];
                let n_score = best_score.entry(n).or_insert(std::u32::MAX);
                if possible_score < *n_score {
                    *n_score = possible_score;
                    if !best_from.contains_key(&n) {
                        todo.push(Node {
                            position: n,
                            score: possible_score + self.heur_score(n),
                        });
                    }
                    best_from.insert(n, position);
                }
            }
        }

        panic!("Never reached goal");
    }

    fn get_best_path_score(&self) -> u32 {
        let best_path = self.a_star();

        best_path
            .iter()
            .map(|(x, y)| self.risk_levels[*y as usize][*x as usize])
            .sum::<u32>()
            - self.risk_levels[0][0]
    }
}

impl Day for Day15 {
    fn part1(&self) -> String {
        let grid = Grid::new(&self.input, 1);

        grid.get_best_path_score().to_string()
    }

    fn part2(&self) -> String {
        let grid = Grid::new(&self.input, 5);

        grid.get_best_path_score().to_string()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT1: &str = r#"1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581"#;

    fn get_day(input_num: u8) -> Day15 {
        let inp = match input_num {
            0 => include_str!("../inputs/day15.txt"),
            1 => INPUT1,
            _ => panic!(),
        };

        Day15 {
            input: inp.split('\n').map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn part1_example1() {
        let d = get_day(1);

        assert_eq!("40", d.part1());
    }

    #[test]
    fn part2_example1() {
        let d = get_day(1);

        assert_eq!("315", d.part2());
    }

    #[test]
    fn part1() {
        let d = get_day(0);

        assert_eq!("390", d.part1());
    }

    #[test]
    fn part2() {
        let d = get_day(0);

        assert_eq!("2814", d.part2());
    }
}
