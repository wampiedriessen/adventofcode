use std::io;
use std::io::Read;
use lazy_static::lazy_static;
use regex::Regex;

#[derive(Debug)]
struct Vector {
    y: i64,
    x: i64
}

lazy_static! {
    static ref day13_re: Regex = Regex::new(
        r"Button A: X(?<ax>(\+|-)[0-9]+), Y(?<ay>(\+|-)[0-9]+)
Button B: X(?<bx>(\+|-)[0-9]+), Y(?<by>(\+|-)[0-9]+)
Prize: X=(?<prizex>[0-9]+), Y=(?<prizey>[0-9]+)").unwrap();
}

fn calc_machines(input: &str, bonus: i64) -> i64 {
    day13_re.captures_iter(&input)
        .map(|capt| {
            let but_a = Vector { y: capt["ay"].parse().unwrap(), x: capt["ax"].parse().unwrap() };
            let but_b = Vector { y: capt["by"].parse().unwrap(), x: capt["bx"].parse().unwrap() };

            let prize = Vector { y: capt["prizey"].parse().unwrap(), x: capt["prizex"].parse().unwrap() };

            calc_machine(&but_a, &but_b, &prize)
        }
        )
        .sum::<i64>()
}

fn calc_machine(but_a: &Vector, but_b: &Vector, prize: &Vector) -> i64 {
    println!("but_a: {:?}, but_b: {:?}, prize: {:?}", but_a, but_b, prize);

    for a in 0..100 {
        let xa = a * but_a.x;
        let ya = a * but_a.y;

        for b in 0..100 {
            let xb = b * but_b.x;
            let yb = b * but_b.y;

            if (xa + xb == prize.x) && (ya + yb == prize.y) {
                println!("{a} x {b}");
                return a * 3 + b;
            }
        }
    }

    0
}

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;


    println!("Part 1: {}", calc_machines(&input, 0));
    println!("Part 1: {}", calc_machines(&input, 10000000000000));

    Ok(())
}