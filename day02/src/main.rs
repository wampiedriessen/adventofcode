use std::io;
use std::io::prelude::*;

fn next_cur1(current: i32, kar: char) -> i32 {
    return match kar {
        'U' => if current > 3 { current-3 } else { current },
        'D' => if current < 7 { current+3 } else { current },
        'L' => if ((current - 1) % 3) != 0 { current-1 } else { current },
        'R' => if (current % 3) != 0 { current+1 } else { current },
        _ => panic!("NO!"),
    }
}

fn next_cur2(current: char, kar: char) -> char {
    return match kar {
        'U' => match current {
            '1' => '1',
            '2' => '2',
            '3' => '1',
            '4' => '4',
            '5' => '5',
            '6' => '2',
            '7' => '3',
            '8' => '4',
            '9' => '9',
            'A' => '6',
            'B' => '7',
            'C' => '8',
            'D' => 'B',
            _ => '0',
        },
        'D' => match current {
            '1' => '3',
            '2' => '6',
            '3' => '7',
            '4' => '8',
            '5' => '5',
            '6' => 'A',
            '7' => 'B',
            '8' => 'C',
            '9' => '9',
            'A' => 'A',
            'B' => 'D',
            'C' => 'C',
            'D' => 'D',
            _ => '0',
        },
        'L' => match current {
            '1' => '1',
            '2' => '2',
            '3' => '2',
            '4' => '3',
            '5' => '5',
            '6' => '5',
            '7' => '6',
            '8' => '7',
            '9' => '8',
            'A' => 'A',
            'B' => 'A',
            'C' => 'B',
            'D' => 'D',
            _ => '0',
        },
        'R' => match current {
            '1' => '1',
            '2' => '3',
            '3' => '4',
            '4' => '4',
            '5' => '6',
            '6' => '7',
            '7' => '8',
            '8' => '9',
            '9' => '9',
            'A' => 'B',
            'B' => 'C',
            'C' => 'C',
            'D' => 'D',
            _ => '0',
        },
        _ => panic!("NO!"),
    }
}

fn main() {
    println!("Input:");

    let stdin = io::stdin();

    let mut current = '5';

    for line in stdin.lock().lines() {

        for kar in line.unwrap().chars() {
            current = next_cur2(current, kar);
        }
        println!("Current: {}", current);

    }
    println!("Current: {}", current);
        

}
