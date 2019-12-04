fn parse(input: &str) -> (u32, u32) {
    let split: Vec<_> = input.split('-').collect();

    return (split[0].parse().unwrap(), split[1].parse().unwrap());
}

fn validpass1(pass: u32) -> bool {
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

fn validpass2(pass: u32) -> bool {
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

pub fn part1() -> u32 {
    let input = include_str!("../inputs/day04.txt").trim();

    return run1(input);
}

pub fn part2() -> u32 {
    let input = include_str!("../inputs/day04.txt").trim();

    return run2(input);
}

fn run1(input:&str) -> u32 {
    let (min, max) = parse(input);

    let mut passfound = 0;

    for pass in min..max {
        if validpass1(pass) {
            passfound += 1;
        }
    }

    return passfound;
}

fn run2(input:&str) -> u32 {
    let (min, max) = parse(input);

    let mut passfound = 0;

    for pass in min..max {
        if validpass2(pass) {
            passfound += 1;
        }
    }

    return passfound;
}
