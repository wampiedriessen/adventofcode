use crate::Day;

pub struct Day01 {}

struct Dial {
    num: i32
}

impl Dial {
    fn rotate(&mut self, mut num: i32) -> i32 {
        let mut num_clicks = 0;

        loop {
            match num {
                x if x > 0 => { num -= 1; self.num += 1; }
                x if x < 0 => { num += 1; self.num -= 1; }
                _ => {}
            };
            self.num = (self.num + 100) % 100;
            if self.num == 0 { num_clicks += 1; }
            if num == 0 { break; }
        }

        num_clicks
    }
}

impl Day for Day01 {
    fn run1(&self, input: &str) -> String {
        let mut dial = Dial { num: 50 };

        let mut count = 0;

        for line in input.lines() {
            if line.is_empty() { continue; }
            let line_num = line.replace("L", "-").replace("R", "").parse::<i32>().unwrap();
            dial.rotate(line_num);
            if dial.num == 0 { count += 1; }
        }

        format!("{}", count)
    }


    fn run2(&self, input: &str) -> String {
        let mut dial = Dial { num: 50 };

        let mut count = 0;

        for line in input.lines() {
            if line.is_empty() { continue; }
            let line_num = line.replace("L", "-").replace("R", "").parse::<i32>().unwrap();
            count += dial.rotate(line_num);
        }

        format!("{}", count)
    }
}

#[cfg(test)]
mod tests {
    use crate::Day;
    use super::Day01;

    const INPUT1: &'static str = "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
";
    const DAY: Day01 = Day01 {};

    #[test]
    fn test1() {
        assert_eq!("3", DAY.run1(INPUT1))
    }

    #[test]
    fn test2() {
        assert_eq!("6", DAY.run2(INPUT1))
    }
}