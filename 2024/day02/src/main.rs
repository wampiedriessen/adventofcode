use std::io;
use std::io::Read;

fn main() -> io::Result<()> {
    let mut list: Vec<Vec<i32>> = Vec::new();

    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;
    for line in input.lines() {
        list.push(line.split_ascii_whitespace().map(|s| s.parse().unwrap()).collect());
    }

    part1(&list);
    part2(&list);

    Ok(())
}

fn growing_or_shrinking(list: &Vec<i32>) -> bool {
    let not_growing = list.windows(2)
        .any(|w| w[0] >= w[1] || w[0].abs_diff(w[1]) > 3);
    let not_shrinking = list.windows(2)
        .any(|w| w[0] <= w[1] || w[0].abs_diff(w[1]) > 3);

    !(not_growing && not_shrinking)
}

fn part1(lists: &Vec<Vec<i32>>) {
    let mut count_valid = 0;

    for list in lists {
        if growing_or_shrinking(list) {
            count_valid += 1;
        }
    }

    println!("Part 1: {}", count_valid);
}

fn reduce_lists(list: &Vec<i32>) -> Vec<Vec<i32>> {
    let mut result = Vec::new();
    for i in 0..list.len() {
        let mut newlist = list.clone();
        newlist.remove(i);
        result.push(newlist);
    }

    result
}

fn part2(lists: &Vec<Vec<i32>>) {
    let mut count_valid = 0;

    for list in lists {
        if growing_or_shrinking(list) {
            count_valid += 1;
        } else {
            for reduced_list in reduce_lists(list) {
                if growing_or_shrinking(&reduced_list) {
                    count_valid += 1;
                    break;
                }
            }
        }
    }

    println!("Part 2: {}", count_valid);
}
