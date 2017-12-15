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

fn main() {
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
    part1(gen_a, gen_b);
    part2(gen_a, gen_b);
}

fn part1(mut gen_a:Generator, mut gen_b:Generator) {
    let mut matchcount = 0;

    for _ in 0..40000000 {
        let a = gen_a.next();
        let b = gen_b.next();

        if (a & 0xFFFF) == (b & 0xFFFF) {
            matchcount += 1;
        }
    }

    println!("Part 1: {:?}", matchcount);
}

fn part2(mut gen_a:Generator, mut gen_b:Generator) {
    let mut matchcount = 0;

    for _ in 0..5000000 {
        let a = gen_a.nextloop();
        let b = gen_b.nextloop();

        if (a & 0xFFFF) == (b & 0xFFFF) {
            matchcount += 1;
        }
    }

    println!("Part 2: {:?}", matchcount);
}