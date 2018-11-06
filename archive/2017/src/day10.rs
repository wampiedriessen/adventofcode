use std::fmt::Write;

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn part1_sample_test() {
    assert_eq!(12, run1("3,4,1,5", 5));
  }

  #[test]
  fn part2_sample_test() {
    assert_eq!("a2582a3a0e66e6e86e3812dcb672a272", run2(""));
    assert_eq!("33efeb34ea91902bb2f59c9920caa6cd", run2("AoC 2017"));
    assert_eq!("3efbe78a8d82f29979031a4aa0b16a9d", run2("1,2,3"));
    assert_eq!("63960835bcdc130f0b66d7ff4f6a5a8e", run2("1,2,4"));
  }

  #[test]
  fn part1_test() {
    assert_eq!(4480, part1());
  }

  #[test]
  fn part2_test() {
    assert_eq!("c500ffe015c83b60fad2e4b7d59dabc4", part2());
  }
}

pub fn part1() -> u32 {
    let input = include_str!("../inputs/day10.txt").trim();

    return run1(input, 256);
}

pub fn part2() -> String {
    let input = include_str!("../inputs/day10.txt").trim();

    return run2(input);
}

fn run1(input: &str, len:u16) -> u32 {
    let mut list: Vec<u8> = gen_hash_list(len);
    let mut index:u8 = 0;
    let mut skipsize:u8 = 0;

    for length in input.split(",") {
        let len = length.parse().unwrap();

        hash_step(&mut list, &mut index, &mut skipsize, len);
    }

    return list[0] as u32 * list[1] as u32;
}

fn run2(input: &str) -> String {
    let mut list:Vec<u8> = gen_hash_list(256);
    let mut index:u8 = 0;
    let mut skipsize:u8 = 0;

    let mut bytes:Vec<u8> = input.to_string().into_bytes();
    bytes.push(17);
    bytes.push(31);
    bytes.push(73);
    bytes.push(47);
    bytes.push(23);

    for _ in 0..64 {
        for len in &bytes {
            hash_step(&mut list, &mut index, &mut skipsize, *len);
        }
    }

    let dh = dense_hash(&list);
    let mut hash: String = String::new();
    for &byte in dh.iter() {
        write!(&mut hash, "{:02x}", byte).unwrap();
    }
    return hash;
}

fn gen_hash_list(len:u16) -> Vec<u8> {
    let prelist: Vec<u16> = (0..len).collect();
    let list: Vec<u8> = prelist.iter().map(|&x| x as u8).collect();
    return list;
}

fn hash_step(v: &mut Vec<u8>, index:&mut u8, skipsize:&mut u8, len: u8) {
    let start = index.clone();
    for i in 0..(len / 2) {
        let a = start.wrapping_add(i) as usize % v.len();
        let b = start.wrapping_add(len).wrapping_sub(i).wrapping_sub(1) as usize % v.len();
        v.swap(a,b);
    }

    *index = index.wrapping_add(len.clone()).wrapping_add(*skipsize);
    *skipsize = skipsize.wrapping_add(1);
}

fn dense_hash(v: &Vec<u8>) -> [u8; 16] {
    let mut hash:[u8; 16] = [0; 16];

    for i in 0..16 {
        let mut val:u8 = v[i*16];
        for j in 1..16 {
            val ^= v[i*16 + j];
        }
        hash[i] = val;
    }

    return hash;
}