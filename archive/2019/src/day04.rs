use super::Day;

pub struct Day04 {
  input: (u32, u32)
}

impl Day for Day04 {
  fn new(input: &str) -> Day04 {
    let split: Vec<_> = input.split('-').collect();

    Day04 {
      input: (split[0].parse().unwrap(), split[1].parse().unwrap()),
    }
  }

  fn part1(&self) -> Box<dyn std::fmt::Display> {
    return Box::new(self.run1());
  }

  fn part2(&self) -> Box<dyn std::fmt::Display> {
    return Box::new(self.run2());
  }
}

// -- Privates
impl Day04 {
    fn validpass1(&self, pass: u32) -> bool {
        let mut seendouble = false;
        let mut workpass = pass;

        let mut i = 10_u32.pow(5);

        let mut last = pass / i;
        workpass -= last * i;

        while i != 1 {
            i /= 10;

            let new = workpass / i;
            workpass -= new * i;

            if new < last {
                return false;
            }
            if new == last {
                seendouble = true;
            }

            last = new;
        }

        return seendouble;
    }

    fn validpass2(&self, pass: u32) -> bool {
        let mut validdouble = false;
        let mut seendouble = false;
        let mut seentriple = false;
        let mut workpass = pass;

        let mut i = 10_u32.pow(5);

        let mut last = pass / i;
        workpass -= last * i;

        while i != 1 {
            i /= 10;

            let new = workpass / i;
            workpass -= new * i;

            if new < last {
                return false;
            }
            if new == last {
                if seendouble {
                    seentriple = true;
                }
                seendouble = true;
            }
            else
            {
                if seendouble && !seentriple {
                    validdouble = true;
                }
                seendouble = false;
                seentriple = false;
            }

            last = new;
        }

        if seendouble && !seentriple {
            validdouble = true;
        }

        return validdouble;
    }

    fn run1(&self) -> u32 {
        let (min, max) = self.input;

        let mut passfound = 0;

        for pass in min..max {
            if self.validpass1(pass) {
                passfound += 1;
            }
        }

        return passfound;
    }

    fn run2(&self) -> u32 {
        let (min, max) = self.input;

        let mut passfound = 0;

        for pass in min..max {
            if self.validpass2(pass) {
                passfound += 1;
            }
        }

        return passfound;
    }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn part1_test() {
    let input = include_str!("../inputs/day04.txt").trim();
    let day = Day04::new(input);

    assert_eq!("966", format!("{}", day.part1()));
  }

  #[test]
  fn part2_test() {
    let input = include_str!("../inputs/day04.txt").trim();
    let day = Day04::new(input);

    assert_eq!("628", format!("{}", day.part2()));
  }
}