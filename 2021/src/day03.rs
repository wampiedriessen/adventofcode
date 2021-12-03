use crate::Day;

pub struct Day03 {
    pub input: Vec<String>,
}

impl Day03 {
    fn bit_criteria_loop(&self, most_common: char, least_common: char) -> u32 {
        let bitlen = self.input[0].len() as u32;
        let mut work_list = self.input.clone();

        // repeat process as long as we have readings
        loop {
            // left to right
            for i in 0..bitlen {
                // count bits equal to one
                let mut countbits = 0;
                for reading in &work_list {
                    if reading.as_bytes()[i as usize] == b'1' {
                        countbits += 1;
                    }
                }
    
                let wlen = &work_list.len();
                // if true, '1' is most common
                let keepchar = if countbits*2 >= *wlen { most_common } else { least_common };
    
                work_list.retain(|x| x.as_bytes()[i as usize] == keepchar as u8);

                if work_list.len() == 1 {
                    return u32::from_str_radix(work_list.pop().unwrap().as_str(), 2).unwrap();
                }
            }
        }

    }
}

impl Day for Day03 {
    fn part1(&self) -> String {
        let bytes: Vec<u32> = self.input.iter().map(|x| u32::from_str_radix(x, 2).unwrap()).collect();
        let bitlen = self.input[0].len() as u32;
        
        let mut gamma = 0;
        let mut epsilon = 0;
        
        for index in 0..bitlen {
            let bit = 2u32.pow(index);
            let mut countbits = 0;
            for reportline in &bytes {
                if (reportline & bit) != 0 {
                    countbits += 1;
                }
            }
            
            if countbits*2 > bytes.len() {
                epsilon += bit;
            } else {
                gamma += bit;
            }
        }
        
        (gamma * epsilon).to_string()
    }
    
    fn part2(&self) -> String {
        let oxy = self.bit_criteria_loop('1', '0');
        let co2 = self.bit_criteria_loop('0', '1');

        (oxy * co2).to_string()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const INPUT: &str = "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010";

    fn get_day(input_num: u8) -> Day03 {
        let inp = match input_num {
            0 => include_str!("../inputs/day03.txt"),
            1 => INPUT,
            _ => panic!(),
        };

        Day03 {
            input: inp.split('\n').map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn part1_example() {
        let d = get_day(1);

        assert_eq!("198", d.part1());
    }

    #[test]
    fn part2_example() {
        let d = get_day(1);

        assert_eq!("230", d.part2());
    }

    #[test]
    fn part1() {
        let d = get_day(0);

        assert_eq!("3985686", d.part1());
    }

    #[test]
    fn part2() {
        let d = get_day(0);

        assert_eq!("2555739", d.part2());
    }
}
