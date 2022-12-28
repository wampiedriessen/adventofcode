use std::collections::HashSet;
use std::str::FromStr;
use crate::Day;

pub struct Day09 {
    pub input: Vec<String>,
}

enum Direction {
    Up,
    Down,
    Left,
    Right,
    RightUp,
    LeftUp,
    LeftDown,
    RightDown,
}

struct Instruction {
    direction: Direction,
    steps: usize
}

impl FromStr for Direction {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "U" => Direction::Up,
            "D" => Direction::Down,
            "R" => Direction::Right,
            "L" => Direction::Left,
            _ => panic!("Cannot give this direction")
        })
    }
}

impl FromStr for Instruction {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let spl: Vec<&str> = s.split_whitespace().collect();

        Ok(Instruction{
            direction: spl[0].parse().unwrap(),
            steps: spl[1].parse().unwrap()
        })
    }
}

struct Rope {
    knots: Vec<(i32, i32)>,
    numknots: usize
}

impl Rope {
    fn new(knots: usize) -> Rope {
        Rope {
            knots: vec![(0, 0); knots],
            numknots: knots
        }
    }

    fn tail(&self) -> (i32, i32) {
        *self.knots.last().unwrap()
    }

    fn step(&mut self, knotnum: usize, dir: &Direction) {
        let headx: i32;
        let heady: i32;
        {
            let head = self.knots.get_mut(knotnum).unwrap();
            match dir {
                Direction::Up => head.1 += 1,
                Direction::Down => head.1 -= 1,
                Direction::Left => head.0 -= 1,
                Direction::Right => head.0 += 1,

                Direction::LeftUp => {head.0 -= 1; head.1 += 1},
                Direction::RightUp => {head.0 += 1; head.1 += 1},
                Direction::LeftDown => {head.0 -= 1; head.1 -= 1},
                Direction::RightDown => {head.0 += 1; head.1 -= 1},
            };
            headx = head.0;
            heady = head.1;
        }

        // last knot
        if knotnum + 1 == self.numknots { return; }

        let tail = self.knots[knotnum+1];

        // no movement for next knot
        if (headx - tail.0).abs() <= 1 && (heady - tail.1).abs() <= 1 { return; }

        let newdir = match (headx - tail.0, heady - tail.1) {
            (-2, x) if x < 0 => Direction::LeftDown,
            (-2, 0) => Direction::Left,
            (-2, x) if x > 0 => Direction::LeftUp,

            (2, x) if x < 0 => Direction::RightDown,
            (2, 0) => Direction::Right,
            (2, x) if x > 0 => Direction::RightUp,

            (x, -2) if x < 0 => Direction::LeftDown,
            (0, -2) => Direction::Down,
            (x, -2) if x > 0 => Direction::RightDown,

            (x, 2) if x < 0 => Direction::LeftUp,
            (0, 2) => Direction::Up,
            (x, 2) if x > 0 => Direction::RightUp,

            _ => panic!("Should have moved already!")
        };

        self.step(knotnum + 1, &newdir);
    }
}

impl Day09 {
    fn all_instructions(&self, mut snake: Rope) -> String {
        let mut tailpositions = HashSet::new();

        for inp in &self.input {
            let inst: Instruction = inp.parse().unwrap();

            for _ in 0..inst.steps {
                snake.step(0, &inst.direction);
                tailpositions.insert(snake.tail());
            }
        }
        tailpositions.len().to_string()
    }
}

impl Day for Day09 {
    fn part1(&self) -> String {
        let snake = Rope::new(2);

        self.all_instructions(snake)
    }

    fn part2(&self) -> String {
        let snake = Rope::new(10);

        self.all_instructions(snake)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT: &str = "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2";

    const INPUT2: &str = "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20";

    fn get_day(input_num: u8) -> Day09 {
        let inp = match input_num {
            0 => include_str!("../inputs/day09.txt"),
            1 => INPUT,
            2 => INPUT2,
            _ => panic!(),
        };

        Day09 {
            input: inp.lines().map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn part1_example() {
        let d = get_day(1);

        assert_eq!("13", d.part1());
    }

    #[test]
    fn part2_example() {
        let d = get_day(1);

        assert_eq!("1", d.part2());
    }

    #[test]
    fn part2_example2() {
        let d = get_day(2);

        assert_eq!("36", d.part2());
    }


    #[test]
    fn part1() {
        let d = get_day(0);

        assert_eq!("5874", d.part1());
    }

    #[test]
    fn part2() {
        let d = get_day(0);

        assert_eq!("474606", d.part2());
    }
}
