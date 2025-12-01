use std::collections::HashMap;
use std::io;
use std::io::Read;

fn create_design<'a>(design: &'a str, patterns: &Vec<&str>, memory: &mut HashMap<&'a str, usize>) -> usize {
    if memory.contains_key(design) { return memory[design]; }

    let mut possibilities = 0;
    for &pattern in patterns {
        if design == pattern {
            possibilities += 1;
        } else if design.starts_with(pattern) {
            possibilities += create_design(&design[pattern.len()..], patterns, memory);
        }
    }
    memory.insert(design, possibilities);

    possibilities
}

fn main() -> io::Result<()> {
    let mut design_line = String::new();
    io::stdin().read_line(&mut design_line)?;

    let mut pattern_input = String::new();
    io::stdin().read_to_string(&mut pattern_input)?;

    let patterns = design_line.split(",").map(|s| s.trim()).collect();
    let designs = pattern_input.split_ascii_whitespace().collect::<Vec<_>>();

    let mut memory = HashMap::new();
    let mut designs_possible = 0;
    let mut mutations_possible = 0;

    for design in designs {
        let mutations = create_design(design, &patterns, &mut memory);
        if mutations > 0 {
            designs_possible += 1;
            mutations_possible += mutations;
        }
    }

    println!("Part 1: {}", designs_possible);
    println!("Part 2: {}", mutations_possible);

    Ok(())
}
