extern crate rayon;

use self::rayon::prelude::*;
use super::Day;

pub struct Day16 {
  input: Vec<u32>
}

impl Day for Day16 {
  fn new(input: &str) -> Day16 {
    Day16 {
      input: input.chars().map(|c| c.to_digit(10).unwrap() as u32).collect(),
    }
  }

  fn part1(&self) -> Box<dyn std::fmt::Display> {
    return Box::new(self.run1());
  }

  fn part2(&self) -> Box<dyn std::fmt::Display> {
    return Box::new(self.run2());
  }
}

// -- Privates
impl Day16 {

  fn get_last(&self, val: i32) -> u32 {
    val.abs() as u32 % 10
  }

  fn get_val_for_chunks(&self, chunks: std::slice::Chunks<u32>, take: usize) -> i32 {
    let mut negate: i32 = 1;
    let mut sum: i32 = 0;
    for chunk in chunks {
      sum += negate * chunk.iter().take(take).sum::<u32>() as i32;
      negate *= -1;
    }

    sum
  }

  fn fft_100_times(&self) -> u32 {
    let listlen = self.input.len();
    let mut lists: [Vec<u32>; 2] = [self.input.clone(), vec![]];

    for i in 0..100 {
      lists[(i+1)%2] = (0..listlen).collect::<Vec<usize>>()
        .par_iter()
        .map(|&row| {
          let take = row + 1;

          let (_, relevant) = lists[i%2].split_at(row);
      
          let chunks = relevant.chunks(take*2);

          let sum = self.get_val_for_chunks(chunks, take);
      
          self.get_last(sum)
      }).collect();
    }

    self.get_answer(&lists[0], 8, 0)
  }

  fn get_answer(&self, vec: &Vec<u32>, take: u32, skip: usize) -> u32 {
    vec.iter().skip(skip).take(take as usize).enumerate().map(|(ind, v)| *v * (10_u32.pow((take - 1) - ind as u32))).sum()
  }

  fn run1(&self) -> u32 {
    self.fft_100_times()
  }

  fn fft_for_padding_more_than_half(&self, padding: usize, repeats: usize) -> u32 {
    let listlen = self.input.len() * repeats;

    let mut cur: Vec<u32> = self.input.repeat(repeats).iter().skip(padding).map(|x| *x).collect();
    let mut prev: Vec<u32> = cur.clone();

    let remainderlen = listlen - padding;

    for _ in 0..100 {
      for j in (0..remainderlen).rev() {
        if j == (remainderlen-1) {
          cur[j] = prev[j];
        } else {
          cur[j] = (prev[j] + cur[j + 1]) % 10;
        }
      }
      let t = cur;
      cur = prev;
      prev = t;
    }

    self.get_answer(&prev, 8, 0)
  }

  fn run2(&self) -> u32 {
    let message_padding = self.get_answer(&self.input, 7, 0) as usize;

    self.fft_for_padding_more_than_half(message_padding, 10000)
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn part1() {
    let day = Day16::new(include_str!("../inputs/day16.txt"));
    assert_eq!(day.run1(), 85726502);
  }

  #[test]
  fn part1_example1() {
    let day = Day16::new("80871224585914546619083218645595");
    assert_eq!(day.run1(), 24176176);
  }

  #[test]
  fn part1_example2() {
    let day = Day16::new("19617804207202209144916044189917");
    assert_eq!(day.run1(), 73745418);
  }

  #[test]
  fn part1_example3() {
    let day = Day16::new("69317163492948606335995924319873");
    assert_eq!(day.run1(), 52432133);
  }

  #[test]
  fn part2() {
    let day = Day16::new(include_str!("../inputs/day16.txt"));
    assert_eq!(day.run2(), 92768399);
  }

  #[test]
  fn part2_example1() {
    let day = Day16::new("03036732577212944063491565474664");
    assert_eq!(day.run2(), 84462026);
  }

  #[test]
  fn part2_example2() {
    let day = Day16::new("02935109699940807407585447034323");
    assert_eq!(day.run2(), 78725270);
  }

  #[test]
  fn part2_example3() {
    let day = Day16::new("03081770884921959731165446850517");
    assert_eq!(day.run2(), 53553731);
  }

}