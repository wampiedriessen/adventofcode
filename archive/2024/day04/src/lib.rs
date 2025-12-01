use aoc_util::PuzzleDay;

pub struct Day04 {
    list: Vec<Vec<char>>,
}

impl PuzzleDay for Day04 {
    fn new(input: &str) -> Self {
        let mut list = Vec::new();
        for line in input.lines() {
            let mut innerlist = Vec::new();
            for char in line.chars() {
                innerlist.push(char)
            }
            list.push(innerlist);
        }

        Day04 { list }
    }

    fn solve(&mut self) -> Result<(String, String), String> {
        Ok((self.part1().to_string(), self.part2().to_string()))
    }
}

impl Day04 {
    fn part1(&self) -> i32 {
        let list = &self.list;

        let mut sum = 0;
        for y in 0..list.len() {
            // horizontal
            for window in list[y].windows(4) {
                let window_string = String::from_iter(window.iter());
                if window_string == "XMAS" || window_string == "SAMX" {
                    sum += 1;
                }
            }
            for x in 0..list[y].len() {
                if list[y][x] != 'X' { continue; }
                if y < list.len() - 3 {
                    // can go down
                    if x < list[y].len() - 3 {
                        if String::from_iter([list[y+1][x+1], list[y+2][x+2], list[y+3][x+3]].iter()) == "MAS" {
                            sum += 1;
                        }
                    }
                    if x >= 3 {
                        if String::from_iter([list[y+1][x-1], list[y+2][x-2], list[y+3][x-3]].iter()) == "MAS" {
                            sum += 1;
                        }
                    }
                    if String::from_iter([list[y+1][x], list[y+2][x], list[y+3][x]].iter()) == "MAS" {
                        sum += 1;
                    }
                }
                if y >= 3 {
                    // can go up
                    if x < list[y].len() - 3 {
                        if String::from_iter([list[y-1][x+1], list[y-2][x+2], list[y-3][x+3]].iter()) == "MAS" {
                            sum += 1;
                        }
                    }
                    if x >= 3 {
                        if String::from_iter([list[y-1][x-1], list[y-2][x-2], list[y-3][x-3]].iter()) == "MAS" {
                            sum += 1;
                        }
                    }
                    if String::from_iter([list[y-1][x], list[y-2][x], list[y-3][x]].iter()) == "MAS" {
                        sum += 1;
                    }
                }
            }
        }

        sum
    }

    fn part2(&self) -> i32 {
        let list = &self.list;

        let mut sum = 0;
        for y in 1..list.len()-1 {
            for x in 1..list[y].len()-1 {
                if list[y][x] == 'A' {
                    if (list[y-1][x-1] == 'M' && list[y+1][x+1] == 'S' ||
                        list[y-1][x-1] == 'S' && list[y+1][x+1] == 'M') &&
                        (list[y+1][x-1] == 'M' && list[y-1][x+1] == 'S' ||
                            list[y+1][x-1] == 'S' && list[y-1][x+1] == 'M')
                        {
                            sum += 1;
                        }
                }
            }
        }

        sum
    }

}
