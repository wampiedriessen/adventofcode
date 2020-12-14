use super::Day;

pub struct Day08 {
  input: Vec<u8>
}

impl Day for Day08 {
  fn new(input: &str) -> Day08 {
    Day08 {
      input: input.chars().map(|c| c.to_digit(10).unwrap() as u8).collect(),
    }
  }

  fn part1(&self) -> Box<dyn std::fmt::Display> {
    return Box::new(self.run1(25, 6));
  }

  fn part2(&self) -> Box<dyn std::fmt::Display> {
    return Box::new(self.run2(25, 6));
  }
}

// -- Privates
impl Day08 {
  fn run1(&self, width: usize, height: usize) -> u32 {
    let num_layers = (self.input.len() / width) / height;

    let mut zeroes: Vec<u32> = vec![0; num_layers];
    let mut ones: Vec<u32> = vec![0; num_layers];
    let mut twos: Vec<u32> = vec![0; num_layers];

    for l in 0..num_layers {
      for y in 0..height {
        for x in 0..width {
          let pixel = self.input[l*width*height + y*width + x];

          if pixel == 0 {
            zeroes[l] += 1;
          }
          if pixel == 1 {
            ones[l] += 1;
          }
          if pixel == 2 {
            twos[l] += 1;
          }
        }
      }
    }

    let mut min_index = 0;
    let mut ans = ones[0] * twos[0];

    for l in 1..num_layers {
      if zeroes[l] < zeroes[min_index] {
        ans = ones[l] * twos[l];
        min_index = l;
      }
    }

    return ans;
  }

  fn run2(&self, width: usize, height: usize) -> String {
    let num_layers = (self.input.len() / width) / height;

    let mut output = String::new();

    for y in 0..height {
      for x in 0..width {
        let mut out = ' ';
        for l in 0..num_layers {
          let pixel = self.input[l*width*height + y*width + x];
          if pixel == 1 {
            out = '#';
            break;
          } else if pixel == 0 {
            out = ' ';
            break;
          }
        }
        output.push(out);
      }
      output.push('\n');
    }

    return output;
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn part1_example_test() {
    let day = Day08::new("123456789012");

    assert_eq!(1, day.run1(3, 2));
  }

  #[test]
  fn part2_example_test() {
    let day = Day08::new("0222112222120000");

    assert_eq!(" #\n# \n", day.run2(2, 2));
  }

  #[test]
  fn part1() {
    let input = include_str!("../inputs/day08.txt").trim();
    let day = Day08::new(input);
    assert_eq!("2080", format!("{}", day.part1()));
  }

  #[test]
  fn part2() {
    let input = include_str!("../inputs/day08.txt").trim();
    let day = Day08::new(input);
    assert_eq!(
" ##  #  # ###   ##  #   #
#  # #  # #  # #  # #   #
#  # #  # #  # #     # # 
#### #  # ###  #      #  
#  # #  # # #  #  #   #  
#  #  ##  #  #  ##    #  
", format!("{}", day.part2()));
  }
}