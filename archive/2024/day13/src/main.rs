use std::io;
use std::io::Read;
use lazy_static::lazy_static;
use regex::Regex;

#[derive(Debug)]
struct Vector {
    y: i128,
    x: i128
}

lazy_static! {
    static ref day13_re: Regex = Regex::new(
        r"Button A: X(?<ax>(\+|-)[0-9]+), Y(?<ay>(\+|-)[0-9]+)
Button B: X(?<bx>(\+|-)[0-9]+), Y(?<by>(\+|-)[0-9]+)
Prize: X=(?<prizex>[0-9]+), Y=(?<prizey>[0-9]+)").unwrap();
}

fn calc_machines(input: &str, pt2: bool) -> i128 {
    day13_re.captures_iter(&input)
        .map(|capt| {
            let but_a = Vector { y: capt["ay"].parse().unwrap(), x: capt["ax"].parse().unwrap() };
            let but_b = Vector { y: capt["by"].parse().unwrap(), x: capt["bx"].parse().unwrap() };

            let prize = Vector { y: capt["prizey"].parse::<i128>().unwrap(), x: capt["prizex"].parse::<i128>().unwrap() };
            let prize2 = Vector { y: 10000000000000 + capt["prizey"].parse::<i128>().unwrap(), x: 10000000000000 + capt["prizex"].parse::<i128>().unwrap() };

            calc_machine(&but_a, &but_b, if pt2 { &prize2 } else { &prize }, pt2)
        }).sum::<i128>()
}

fn calc_machine(but_a: &Vector, but_b: &Vector, prize: &Vector, pt2: bool) -> i128 {
    // (1)
    // A*K + B*L = Px             -      94A + 22B = 8400
    // A*M + B*N = Py             -      34A + 67B = 5400

    // (2)
    // A*N*K + B*L*N = Px*N          -  6298A + 1474B = 562800
    // A*M*-L + B*N*-L = Py*-L       -  -748A + -1474B = -118800

    // (3)
    // A*(N*K+M*-L) = (Px*N+Py*-L)   - 5550A = 444000

    // From A we calculate B


    let left_a = but_a.x * but_b.y;
    let left_b = but_a.y * -but_b.x;

    let right_a = prize.x * but_b.y;
    let right_b = prize.y * -but_b.x;

    let a = (right_a + right_b) / (left_a + left_b);
    let b = (prize.x - a*but_a.x) / but_b.x;

    // not divisible
    if (right_a + right_b) % (left_a + left_b) != 0 ||
        (prize.x - a*but_a.x) % but_b.x != 0
    {
        return 0;
    }

    if !pt2 && (a > 100 || b > 100) { return 0; }

    3*a + b
}

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    println!("Part 1: {}", calc_machines(&input, false));
    println!("Part 1: {}", calc_machines(&input, true));

    Ok(())
}