use std::io;
use std::io::Read;
use std::str::FromStr;

struct CalibrationEquation {
    total: u64,
    values: Vec<u64>,
}

impl FromStr for CalibrationEquation {
    type Err = ();
    fn from_str(line: &str) -> Result<Self, Self::Err> {
        let split = line.split_once(':').unwrap();
        let total: u64 = split.0.parse().unwrap();
        let values: Vec<u64> = split.1.trim().split_ascii_whitespace().map(|x| x.parse().unwrap()).collect();

        Ok(CalibrationEquation {
            total,
            values
        })
    }
}

impl CalibrationEquation {
    fn is_valid(&self) -> bool {
        if self.values.is_empty() { return false; }
        let slice = self.values.as_slice();
        Self::recurse_solve(self.total, slice[0], &slice[1..], false)
    }

    fn is_valid_p2(&self) -> bool {
        if self.values.is_empty() { return false; }
        let slice = self.values.as_slice();
        Self::recurse_solve(self.total, slice[0], &slice[1..], true)
    }

    fn recurse_solve(expected_total: u64, current: u64, slice: &[u64], allow_concat: bool) -> bool {
        if current > expected_total { return false;}
        let val = slice[0];

        let mul = current * val;
        let add = current + val;

        let digits = (1..5u32).into_iter().find(|x| val / 10u64.pow(*x) == 0).unwrap();
        let con = current * 10u64.pow(digits) + val;
        if slice.len() == 1 {
            expected_total == mul || expected_total == add || allow_concat && expected_total == con
        }
        else {
            Self::recurse_solve(expected_total, mul, &slice[1..], allow_concat)
                || Self::recurse_solve(expected_total, add, &slice[1..], allow_concat)
                || allow_concat && Self::recurse_solve(expected_total, con, &slice[1..], allow_concat)

        }
    }
}

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    let mut p1_result = 0;
    let mut p2_result = 0;

    for line in input.lines() {
        if line.is_empty() { continue; }
        let equation = CalibrationEquation::from_str(line).unwrap();
        if equation.is_valid() {
            p1_result += equation.total;
        }
        if equation.is_valid_p2() {
            p2_result += equation.total;
        }
    }

    println!("Part 1: {}", p1_result);
    println!("Part 2: {}", p2_result);

    Ok(())
}