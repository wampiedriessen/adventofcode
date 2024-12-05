use std::cmp::Ordering::{Greater, Less, Equal};
use std::collections::{HashMap, HashSet};
use std::io;
use std::io::Read;

struct Sorter {
    before_x: HashMap<u32, HashSet<u32>>,
    after_x: HashMap<u32, HashSet<u32>>,
}

impl Sorter {
    fn ordering(&self, a: &u32, b: &u32) -> std::cmp::Ordering {
        if self.before_x.contains_key(a) && self.before_x.get(a).unwrap().contains(b) {
            return Less;
        }
        if self.after_x.contains_key(a) && self.after_x.get(a).unwrap().contains(b) {
            return Greater;
        }
        if self.before_x.contains_key(b) && self.before_x.get(b).unwrap().contains(a) {
            return Greater;
        }
        if self.after_x.contains_key(b) && self.after_x.get(b).unwrap().contains(a) {
            return Less;
        }
        Equal
    }
    fn is_sorted(&self, list: &Vec<u32>) -> bool {
        list.is_sorted_by(|a, b| self.ordering(a, b) != Less)
    }
    fn sort(&self, list: &Vec<u32>) -> Vec<u32> {
        let mut new_list = list.clone();
        new_list.sort_by(|a, b| self.ordering(a, b));
        new_list
    }
}

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    let mut invalid_manuals = HashSet::new();

    let mut middle_number_sum = 0;

    let mut sorter = Sorter { before_x: HashMap::new(), after_x: HashMap::new() };

    for line in input.lines() {
        if line.contains("|") {
            let (a, b) = line.split_once("|").unwrap();

            let before_entry = sorter.before_x.entry(b.parse::<u32>().unwrap()).or_default();
            before_entry.insert(a.parse::<u32>().unwrap());

            let after_entry = sorter.after_x.entry(a.parse::<u32>().unwrap()).or_default();
            after_entry.insert(b.parse::<u32>().unwrap());
        } else if line.contains(",") {

            let pages: Vec<u32> = line.split(",").map(|x| x.parse::<u32>().unwrap()).collect();
            if sorter.is_sorted(&pages) {
                middle_number_sum += *pages.get(pages.len() / 2).unwrap();
            } else {
                invalid_manuals.insert(pages);
            }

        }
    }

    println!("Part 1: {}", middle_number_sum);

    let mut reordened_middle_number_sum = 0;

    for manual in invalid_manuals {
        let new_manual = sorter.sort(&manual);

        reordened_middle_number_sum += *new_manual.get(new_manual.len() / 2).unwrap()
    }

    println!("Part 2: {}", reordened_middle_number_sum);

    Ok(())
}