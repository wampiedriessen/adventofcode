#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn part1_sample_test() {
    let (mut ga, mut gb) = get_input();
    ga.latest = 65;
    gb.latest = 8921;
    assert_eq!(588, run1(ga, gb));
  }

  #[test]
  fn part2_sample_test() {
    let (mut ga, mut gb) = get_input();
    ga.latest = 65;
    gb.latest = 8921;
    assert_eq!(309, run2(ga, gb));
  }

  #[test]
  fn part1_test() {
    assert_eq!(597, part1());
  }

  #[test]
  fn part2_test() {
    assert_eq!(303, part2());
  }
}


#[derive(Debug, Clone, Copy)]
struct Generator {
    factor:u64,
    modulo:u64,
    latest:u64,
}

impl Generator {
    pub fn next(&mut self) -> u64 {
        self.latest = self.latest * self.factor % 2147483647;
        return self.latest;
    }

    pub fn nextloop(&mut self) -> u64 {
        loop {
            //do:
            self.latest = self.next();
            //while:
            if (self.latest % self.modulo) == 0 {
                break;
            }
        }
        return self.latest;
    }
}

pub fn part1() -> i32 {
    let (ga, gb) = get_input();
    run1(ga, gb)
}

pub fn part2() -> i32 {
    let (ga, gb) = get_input();
    run2(ga, gb)
}

fn run1(mut gen_a:Generator, mut gen_b:Generator) -> i32 {
    let mut matchcount = 0;

    for _ in 0..40000000 {
        let a = gen_a.next();
        let b = gen_b.next();

        if (a & 0xFFFF) == (b & 0xFFFF) {
            matchcount += 1;
        }
    }

    matchcount
}

fn run2(mut gen_a:Generator, mut gen_b:Generator) -> i32 {
    let mut matchcount = 0;

    for _ in 0..5000000 {
        let a = gen_a.nextloop();
        let b = gen_b.nextloop();

        if (a & 0xFFFF) == (b & 0xFFFF) {
            matchcount += 1;
        }
    }

    matchcount
}

fn get_input() -> (Generator, Generator) {
    let inputa = 516;
    let inputb = 190;
    let gen_a = Generator {
        factor: 16807,
        modulo: 4,
        latest: inputa
    };
    let gen_b = Generator {
        factor: 48271,
        modulo: 8,
        latest: inputb
    };

    (gen_a, gen_b)
}