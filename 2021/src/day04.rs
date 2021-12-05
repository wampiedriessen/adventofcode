use std::collections::HashSet;

use crate::Day;

pub struct Day04 {
    pub input: Vec<String>,
}

#[derive(Debug)]
struct Board {
    grid: [[i64; 5]; 5],
}

impl Board {
    fn has_won(&self, draws: &HashSet<i64>) -> bool {
        for i in 0..5 {
            let mut xc = 0;
            let mut yc = 0;
            for j in 0..5 {
                if draws.contains(&self.grid[i][j]) {
                    xc += 1;
                }
                if draws.contains(&self.grid[j][i]) {
                    yc += 1;
                }
            }
            if xc == 5 || yc == 5 {
                return true;
            }
        }
        false
    }

    fn get_sum_left(&self, draws: HashSet<i64>) -> i64 {
        let mut sum = 0;

        for l in self.grid {
            for n in l {
                if !draws.contains(&n) {
                    sum += n
                }
            }
        }
        sum
    }
}

impl Day04 {
    fn read_board(&self, board: &[String]) -> Board {
        let mut grid: [[i64; 5]; 5] = [[-1; 5]; 5];

        for y in 0..5 {
            let line: Vec<i64> = board[y]
                .split_whitespace()
                .map(|c| c.parse().unwrap())
                .collect();
            for x in 0..5 {
                grid[y][x] = line[x];
            }
        }
        Board { grid }
    }

    fn read_boards(&self) -> (Vec<i64>, Vec<Board>) {
        let mut draws: Vec<i64> = self.input[0]
            .split(',')
            .map(|c| c.parse().unwrap())
            .collect();
        draws.reverse(); // make it into a queue

        let boards = self.input[1..]
            .chunks(6)
            .map(|board| self.read_board(&board[1..]))
            .collect();

        (draws, boards)
    }
}

impl Day for Day04 {
    fn part1(&self) -> String {
        let (mut drawqueue, boards) = self.read_boards();

        let mut drawn = HashSet::new();
        let mut ended = false;
        let mut lastdraw = -1;
        let mut winner = &boards[0];
        while !drawqueue.is_empty() && !ended {
            lastdraw = drawqueue.pop().unwrap();

            let _ = &drawn.insert(lastdraw);
            for board in &boards {
                if board.has_won(&drawn) {
                    ended = true;
                    winner = board;
                    break;
                }
            }
        }

        let winnersum = winner.get_sum_left(drawn);

        (winnersum * lastdraw).to_string()
    }

    fn part2(&self) -> String {
        let (mut drawqueue, mut boards) = self.read_boards();

        let mut drawn = HashSet::new();
        let mut lastdraw = -1;
        while boards.len() > 1 {
            lastdraw = drawqueue.pop().unwrap();

            let _ = &drawn.insert(lastdraw);

            boards.retain(|b| !b.has_won(&drawn));
        }

        let loser = boards.pop().unwrap();

        // loser needs to play to the end:
        while !loser.has_won(&drawn) {
            lastdraw = drawqueue.pop().unwrap();

            let _ = &drawn.insert(lastdraw);
        }

        let losersum = loser.get_sum_left(drawn);

        (losersum * lastdraw).to_string()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT: &str = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7";

    fn get_day(input_num: u8) -> Day04 {
        let inp = match input_num {
            0 => include_str!("../inputs/day04.txt"),
            1 => INPUT,
            _ => panic!(),
        };

        Day04 {
            input: inp.split('\n').map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn part1_example() {
        let d = get_day(1);

        assert_eq!("4512", d.part1());
    }

    #[test]
    fn part2_example() {
        let d = get_day(1);

        assert_eq!("1924", d.part2());
    }

    #[test]
    fn part1() {
        let d = get_day(0);

        assert_eq!("8442", d.part1());
    }

    #[test]
    fn part2() {
        let d = get_day(0);

        assert_eq!("4590", d.part2());
    }
}
