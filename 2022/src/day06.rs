use crate::Day;

pub struct Day06 {
    pub input: Vec<String>,
}

fn detect_marker(window: &[char], num: usize) -> bool {
    for i in 0..num {
        for j in (i+1)..num {
            if window[i] == window[j] {
                return false;
            }
        }
    }
    return true;
}

fn get_marker_pos(signal: &String, num: usize) -> String {
    let characters: Vec<char> = signal.chars().collect();
    let markersearch = characters.windows(num);

    let mut i = 0;
    for window in markersearch {
        if detect_marker(window, num) {
            break;
        }
        i += 1;
    }

    (i + num).to_string()
}

impl Day for Day06 {
    fn part1(&self) -> String {
        let signal = self.input.get(0).unwrap();

        get_marker_pos(signal, 4)
    }

    fn part2(&self) -> String {
        let signal = self.input.get(0).unwrap();

        get_marker_pos(signal, 14)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT: &str = "mjqjpqmgbljsphdztnvjfqwrcgsmlb";

    fn get_day(input_num: u8) -> Day06 {
        let inp = match input_num {
            0 => include_str!("../inputs/day06.txt"),
            1 => INPUT,
            _ => panic!(),
        };

        Day06 {
            input: inp.lines().map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn part1_example() {
        let d = get_day(1);

        assert_eq!("7", d.part1());
    }

    #[test]
    fn part2_example() {
        let d = get_day(1);

        assert_eq!("19", d.part2());
    }

    #[test]
    fn part1() {
        let d = get_day(0);

        assert_eq!("1658", d.part1());
    }

    #[test]
    fn part2() {
        let d = get_day(0);

        assert_eq!("2260", d.part2());
    }
}
