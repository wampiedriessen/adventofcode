use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;

fn main() {
    let mut input = String::new();
    let f = File::open("input.txt").expect("no file?");
    let mut reader = BufReader::new(f);

    reader.read_line(&mut input).expect("Failed to read line");

    let mut sum = 0;
    let mut last:char = ' ';
    let characters = input.trim().chars();
    let mut first:char = ' ';

    for s in characters {
        if first == ' '
        {
            first = s;
        }
        if s == last
        {
            sum += s.to_string().parse::<u32>().unwrap();
        }
        last = s;
    }

    if first == last
    {
        sum += last.to_string().parse::<u32>().unwrap();;
    }

    println!("Part 1: {:?}", sum);

    let length = input.trim().len();
    let lookahead = length / 2;
    sum = 0;
    for i in 0..length {
        if input.chars().nth(i) == input.chars().nth((i+lookahead) % length)
        {
            sum += input.chars().nth(i).unwrap().to_string().parse::<u32>().unwrap();
        }
    }

    println!("Part 2: {:?}", sum);
}
