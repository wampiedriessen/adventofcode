use std::collections::HashMap;
use std::io;
use std::io::Read;

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    let stones: Vec<u64> = input.split_ascii_whitespace().map(|x| x.parse::<u64>().unwrap()).collect();

    let mut memory = HashMap::new();
    let mut p1_sum = 0;
    for stone in &stones {
        p1_sum += recurse_blink(&mut memory, *stone, 25);
    }

    println!("Part 1: {}", p1_sum);

    let mut p2_sum = 0;
    for stone in &stones {
        p2_sum += recurse_blink(&mut memory, *stone, 75);
    }

    println!("Part 2: {}", p2_sum);

    Ok(())
}

fn recurse_blink(memory: &mut HashMap<(usize, u64), usize>, stone: u64, blinks_to_go: usize) -> usize {
    if blinks_to_go == 0 { return 1; }

    if let Some(val) = memory.get(&(blinks_to_go, stone)) { return *val; }

    let newval =
    if stone == 0 {
        recurse_blink(memory, 1, blinks_to_go - 1)
    } else {
        let digits = stone.ilog10() + 1;
        if digits % 2 == 0 {
            let pow = 10u64.pow(digits/2);
            recurse_blink(memory, stone / pow, blinks_to_go - 1)
                + recurse_blink(memory, stone % pow, blinks_to_go - 1)
        } else {
            recurse_blink(memory, stone * 2024, blinks_to_go - 1)
        }
    };

    memory.insert((blinks_to_go, stone), newval);
    newval
}
