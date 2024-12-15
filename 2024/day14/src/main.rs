use std::io;
use std::io::Read;
use lazy_static::lazy_static;
use regex::Regex;

#[derive(Debug, Clone)]
struct Robot {
    x: i64,
    y: i64,

    vx: i64,
    vy: i64,
}

lazy_static! {
    static ref day14_re: Regex = Regex::new("p=(?<x>-?[0-9]+),(?<y>-?[0-9]+) v=(?<vx>-?[0-9]+),(?<vy>-?[0-9]+)").unwrap();
}

const BOUNDX: i64 = 101;
const BOUNDY: i64 = 103;

fn step(bots: &mut Vec<Robot>) {
    for bot in bots {
        bot.x = bot.x + bot.vx;
        bot.y = bot.y + bot.vy;
        if bot.x >= BOUNDX {
            bot.x -= BOUNDX;
        }
        if bot.y >= BOUNDY {
            bot.y -= BOUNDY;
        }
        if bot.x < 0 {
            bot.x += BOUNDX;
        }
        if bot.y < 0 {
            bot.y += BOUNDY;
        }
    }
}

fn part1(bots: &Vec<Robot>) -> usize {
    let mut bots = bots.clone();

    for _ in 0..100 {
        step(&mut bots);
    }

    let mut qs = [0; 4];
    for bot in &bots {
        match (bot.x, bot.y) {
            (x, y) if x < BOUNDX / 2 && y < BOUNDY / 2 => {
                qs[0] += 1;
            }
            (x, y) if x > BOUNDX / 2 && y < BOUNDY / 2 => {
                qs[1] += 1;
            }
            (x, y) if x < BOUNDX / 2 && y > BOUNDY / 2 => {
                qs[2] += 1;
            }
            (x, y) if x > BOUNDX / 2 && y > BOUNDY / 2 => {
                qs[3] += 1;
            }
            _ => { }
        }
    }

    qs[0] * qs[1] * qs[2] * qs[3]
}

fn print_bots_if_tree(bots: &Vec<Robot>) -> bool {
    let coords = bots.iter().map(|b| (b.x, b.y)).collect::<Vec<_>>();
    let mut seenconsecutive_max = 0;
    for y in 0..BOUNDY {
        let mut seenconsecutive = 0;
        for x in 0..BOUNDX {
            if coords.contains(&(x, y)) {
                seenconsecutive += 1;
                seenconsecutive_max = seenconsecutive_max.max(seenconsecutive)
            } else {
                seenconsecutive = 0;
            }
        }
    }
    if seenconsecutive_max >= 30 {
        for y in 0..BOUNDY {
            for x in 0..BOUNDX {
                print!("{}", if coords.contains(&(x, y)) { "#" } else { "." });
            }
            println!();
        }
        return true;
    }
    false
}

fn part2(bots: &Vec<Robot>) -> usize {
    let mut bots = bots.clone();

    let mut s = 0;
    loop {
        if print_bots_if_tree(&bots) {
            break;
        };
        step(&mut bots);
        s += 1;

        if s % 1000 == 0 {
            println!("{}", s);
        }
    }

    s
}

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    let mut bots = Vec::new();

    for capt in day14_re.captures_iter(&input) {
        bots.push(Robot {
           x: capt["x"].parse::<i64>().unwrap(),
           y: capt["y"].parse::<i64>().unwrap(),
           vx: capt["vx"].parse::<i64>().unwrap(),
           vy: capt["vy"].parse::<i64>().unwrap(),
        });
    }

    println!("Part 1: {}", part1(&bots));
    println!("Part 2: {}", part2(&bots));

    Ok(())
}