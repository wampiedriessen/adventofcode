use std::io::prelude::*;
use std::io::BufReader;
use std::fs::File;

use std::mem;

const height: usize = 6;
const width: usize = 50;

fn rekt_cmd(cmdline: &mut std::str::SplitWhitespace, screen: &mut Vec<Vec<bool>>) {
    let split: Vec<&str> = cmdline.next().unwrap().split("x").collect();
    let cols: usize = split[0].parse().expect("NaN");
    let rows: usize = split[1].parse().expect("NaN");

    for col in 0..cols {
        for row in 0..rows {
            screen[row][col] = true;
        }
    }
}

fn row_cmd(cmdline: &mut std::str::SplitWhitespace, screen: &mut Vec<Vec<bool>>) {
    let Yis = cmdline.next().unwrap();
    let rownr: i32 = Yis[2..].parse().expect("NaN!");

    let amountstr = cmdline.skip(1).next().unwrap();
    let amount: i32 = amountstr.parse().expect("NaN!");

    for i in 0..amount {
        let mut last: bool = screen[rownr as usize][0];
        for x in 1..width {
            mem::swap(&mut screen[rownr as usize][x], &mut last);
        }
        mem::swap(&mut screen[rownr as usize][0], &mut last);
    }

}

fn column_cmd(cmdline: &mut std::str::SplitWhitespace, screen: &mut Vec<Vec<bool>>) {
    let Xis = cmdline.next().unwrap();
    let colnr: i32 = Xis[2..].parse().expect("NaN!");

    let amountstr = cmdline.skip(1).next().unwrap();
    let amount: i32 = amountstr.parse().expect("NaN!");

    for i in 0..amount {
        let mut last: bool = screen[0][colnr as usize];
        for x in 1..height {
            mem::swap(&mut screen[x][colnr as usize], &mut last);
        }
        mem::swap(&mut screen[0][colnr as usize], &mut last);
    }

}

fn main() {
    let f = File::open("input.txt").expect("no file?");
    let reader = BufReader::new(f);

    let mut screen: Vec<Vec<bool>> = vec![vec![false; width]; height];

    for line in reader.lines() {

        let linestring = line.unwrap();
        let mut slices = linestring.split_whitespace();
        let cmd = slices.next().unwrap();
        if cmd == "rect" {
            rekt_cmd(&mut slices, &mut screen);
        } else if cmd == "rotate" {
            let subcmd = slices.next().unwrap();
            if subcmd == "row" {
                row_cmd(&mut slices, &mut screen);
            } else if subcmd == "column" {
                column_cmd(&mut slices, &mut screen);
            }
        } else {
            break;
        }
    }


    let mut litpixels = 0;
    for col in screen {
        for row in col {
            if row {
                print!("#");
                litpixels += 1;
            } else {
                print!(".");
            }
        }
        print!("\n");
    }
    println!("Lit pixels: {}", litpixels);
}