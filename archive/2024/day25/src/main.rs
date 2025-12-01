use std::io;
use std::io::Read;

fn parseblock(block: &str) -> [u8; 5] {
    let mut result = [0; 5];

    for line in block.lines() {
        for (i, ch) in line.chars().enumerate() {
            result[i] += if ch == '#' { 1 } else { 0 };
        }
    }

    result
}

fn main() -> Result<(), ()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();

    let mut locks: Vec<[u8; 5]> = Vec::new();
    let mut keys: Vec<[u8; 5]> = Vec::new();

    for block in input.lines().collect::<Vec<&str>>().chunks(8) {
        let block = block.join("\n");
        if block.starts_with("#####") {
            locks.push(parseblock(&block));
        } else {
            keys.push(parseblock(&block));
        }
    }

    let valid_combos = locks
        .iter()
        .map(|lock| {
            keys.iter()
                .map(|key| {
                    if (0..5).into_iter().all(|i| lock[i] + key[i] <= 7) {
                        1
                    } else {
                        0
                    }
                })
                .sum::<usize>()
        })
        .sum::<usize>();

    println!("Part 1: {}", valid_combos);

    Ok(())
}
