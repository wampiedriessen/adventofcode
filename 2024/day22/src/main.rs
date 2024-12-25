use std::io;
use std::io::Read;
use rayon::prelude::*;

fn prune(num: i64) -> i64 {
    num % 16777216
}

fn mix(a: i64, b: i64) -> i64 {
    a ^ b
}

fn next_num(mut num: i64) -> i64 {
    num = prune(mix(num, num * 64));
    num = prune(mix(num, num / 32));
    num = prune(mix(num, num * 2048));

    num
}

fn price(num: i64) -> i64 {
    num % 10
}

fn part1(input: &str) -> i64 {
    let mut sum = 0;
    for line in input.lines() {
        let mut num = line.parse().unwrap();
        for _ in 0..2000 { num = next_num(num); }

        sum += num;
    }

    sum
}

fn bananas_at_signal(signal: &[i64; 4], prices_per_monkey: &Vec<Vec<i64>>, deltas_per_monkey: &Vec<Vec<i64>>) -> i64 {
    deltas_per_monkey
        .par_iter()
        .enumerate()
        .map(|(monkey, deltas)| {
            let mut index = 4;
            for window in deltas.windows(4) {
                if window == signal {
                    return prices_per_monkey[monkey][index];
                }
                index += 1;
            }
            0
        }).sum()
}

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    println!("Part 1: {}", part1(&input));

    let mut prices_per_monkey: Vec<Vec<i64>> = Vec::new();
    let mut deltas_per_monkey: Vec<Vec<i64>> = Vec::new();

    for line in input.lines() {
        let mut num = line.parse().unwrap();

        let mut prices = vec![num];
        let mut deltas = Vec::new();

        for _ in 0..2000 {
            let newnum = next_num(num);
            prices.push(price(newnum));
            deltas.push(price(newnum) - price(num));

            num = newnum;
        }

        prices_per_monkey.push(prices);
        deltas_per_monkey.push(deltas);
    }

    let possible_deltas = Vec::from_iter(-9..=9);

    let most_bananas =
        possible_deltas.par_iter().flat_map(|a| {
            possible_deltas.par_iter().flat_map(|b| {
                possible_deltas.par_iter().flat_map(|c| {
                    possible_deltas.par_iter().map(|d| {
                        let signal = [*a, *b, *c, *d];
                        bananas_at_signal(&signal, &prices_per_monkey, &deltas_per_monkey)
                    })
                })
            })
        }).max().unwrap();

    println!("Part 2: {}", most_bananas);

    Ok(())
}
