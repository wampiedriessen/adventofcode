use std::{io::Error, collections::HashMap};

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let output = match args[1].as_str() {
        "1" => part1(),
        "2" => part2(),
        _ => Ok("No part was given!".to_string())
    }.expect("ERROR!");

    println!("{output}");
}

fn readlines() -> Result<Vec<String>, Error> {
    std::io::stdin().lines().collect()
}

fn max_color_map(line: String) -> (u64, HashMap<String, u64>) {
    let (_, relevant) = line.split_at(5);

    let (gamenum, grab_string) = relevant.split_once(": ").expect("incorrect formatting!");

    let grabs = grab_string.split("; ");

    let mut cube_map = HashMap::new();

    for grab in grabs {
        let cubes = grab.split(", ")
            .map(|c| c.split_once(" ").expect("Incorrect formatting2!"));

        for (a, color) in cubes {
            let amount = a.parse().unwrap();
            let e = cube_map.entry(color.to_string()).or_default();

            if *e < amount {
                *e = amount;
            }
        }

    }

    (gamenum.parse().unwrap(), cube_map)
}

// only 12 red cubes, 13 green cubes, and 14 blue cubes

fn part1() -> Result<String, Error> {
    let max_red = 12;
    let max_green = 13;
    let max_blue= 14;

    let mut total = 0;

    for line in readlines()? {

        let (gamenum, map) = max_color_map(line);

        if *map.get("red").unwrap() <= max_red &&
            *map.get("green").unwrap() <= max_green &&
            *map.get("blue").unwrap() <= max_blue {
            total += gamenum;
        }
    }

    Ok(total.to_string())
}

fn part2() -> Result<String, Error> {
    let mut total = 0;
    
    for line in readlines()? {

        let (_, map) = max_color_map(line);

        total += 
            *map.get("red").unwrap() *
            *map.get("green").unwrap() *
            *map.get("blue").unwrap();
    }

    Ok(total.to_string())
}
