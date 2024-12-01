use std::collections::HashMap;
use std::io;
use std::io::Read;

fn main() -> io::Result<()> {
    let mut list1 = Vec::new();
    let mut list2 = Vec::new();

    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    let mut flip = false;
    for word in input.split_ascii_whitespace() {
        if !flip {
            list1.push(word.parse::<i32>().unwrap());
        } else {
            list2.push(word.parse::<i32>().unwrap());
        }
        flip = !flip;
    }

    part1(&mut list1, &mut list2)?;
    part2(&list1, &list2)?;
    Ok(())
}

fn part1(list1: &mut Vec<i32>, list2: &mut Vec<i32>) -> io::Result<()> {
    list1.sort();
    list2.sort();

    let mut sum = 0;

    for i in 0..list1.len() {
        sum += (list1[i] - list2[i]).abs();
    }
    println!("Part1: {}", sum);
    Ok(())
}

fn part2(list1: &Vec<i32>, list2: &Vec<i32>) -> io::Result<()> {
    let mut scores = HashMap::new();
    for i in list2 {
        *scores.entry(*i).or_insert(0) += i;
    }
    let mut sum = 0;
    for i in list1 {
        sum += scores.get(i).unwrap_or(&0);
    }
    println!("Part2: {}", sum);

    Ok(())
}

