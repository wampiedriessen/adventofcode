fn main() {
    part1();
    part2();
}

fn part1() {
    let inputa = 516;
    let inputb = 190;
    let mut preva:u64 = inputa;
    let mut prevb:u64 = inputb;

    let mut matchcount = 0;

    for _ in 0..40000000 {
        preva = preva * 16807 % 2147483647;
        prevb = prevb * 48271 % 2147483647;

        if (preva & 0xFFFF) == (prevb & 0xFFFF) {
            matchcount += 1;
        }
    }

    println!("Part 1: {:?}", matchcount);
}

fn part2() {
    let inputa = 516;
    let inputb = 190;
    let mut preva:u64 = inputa;
    let mut prevb:u64 = inputb;

    let mut matchcount = 0;

    for _ in 0..5000000 {
        while {
            //do:
            preva = preva * 16807 % 2147483647;
            //while:
            (preva % 4) != 0
        } {}
        while {
            //do:
            prevb = prevb * 48271 % 2147483647;
            //while:
            (prevb % 8) != 0
        } {}

        if (preva & 0xFFFF) == (prevb & 0xFFFF) {
            matchcount += 1;
        }
    }

    println!("Part 2: {:?}", matchcount);
}