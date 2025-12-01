mod day01;
mod day02;

fn main() {
    let days: Vec<(Box<dyn Day>, &'static str)> = vec![
        (Box::new(day01::Day01 {}), include_str!("../inputs/day01.txt")),
        (Box::new(day02::Day02 {}), include_str!("../inputs/day02.txt"))
    ];

    for (index, (day, input)) in days.iter().enumerate() {
        println!("Day {}", index+1);
        println!("p1: {}", day.run1(input));
        println!("p2: {}", day.run2(input));
    }
}

trait Day {
    fn run1(&self, input: &str) -> String;
    fn run2(&self, input: &str) -> String;
}
