use std::collections::{HashMap, HashSet};
use std::io;
use std::io::Read;

fn test(p1: (i32, i32), maxpos: (i32, i32)) -> bool {
    p1.0 >= 0 && p1.0 <= maxpos.0
      && p1.1 >= 0 && p1.1 <= maxpos.1
}

fn calc_nodes(p1: (i32, i32), rest: &[(i32, i32)], maxpos: (i32, i32), all_positions: bool) -> Vec<(i32, i32)> {
    let mut result = Vec::new();

    for p2 in rest {
        let disty = p2.0 - p1.0;
        let distx = p2.1 - p1.1;
        let mut added = true;
        let mut mult = if all_positions { 0 } else { 1 };
        while added {
            added = false;

            let newp1 = (p1.0 - mult*disty, p1.1 - mult*distx);
            let newp2 = (p2.0 + mult*disty, p2.1 + mult*distx);

            if test(newp1, maxpos) { result.push(newp1); added = true; }
            if test(newp2, maxpos) { result.push(newp2); added = true; }

            if !all_positions { break }
            mult += 1;
        }
    }

    result
}

fn calc_antinodes(frequencies: &HashMap<char, Vec<(i32, i32)>>, maxpos: (i32, i32), all_positions: bool) -> usize {
    let mut all_antinodes: HashSet<(i32, i32)> = HashSet::new();
    for (_, fq_antennas) in frequencies {
        let mut antinodes: HashSet<(i32, i32)> = HashSet::new();
        let mut slice = fq_antennas.as_slice();

        while slice.len() > 1 {
            let newnodes = calc_nodes(slice[0], &slice[1..], maxpos, all_positions);
            antinodes.extend(&newnodes);
            slice = &slice[1..];
        }

        all_antinodes.extend(antinodes);
    }

    all_antinodes.len()
}

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    let mut frequencies = HashMap::new();

    let mut maxpos = (0, 0);
    for (y, line) in input.lines().enumerate() {
        for (x, digit) in line.chars().enumerate() {
            let pos = (y as i32, x as i32);
            maxpos = pos;
            if digit == '.' { continue; }
            frequencies.entry(digit).or_insert(Vec::new()).push(pos);
        }
    }

    let antinodes_p1 = calc_antinodes(&frequencies, maxpos, false);
    let antinodes_p2 = calc_antinodes(&frequencies, maxpos, true);

    println!("Part 1: {}", antinodes_p1);
    println!("Part 2: {}", antinodes_p2);

    Ok(())
}