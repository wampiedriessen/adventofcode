#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn part1_sample_test() {
    assert_eq!(8108, run1("flqrgnkx".to_string()));
  }

  #[test]
  fn part2_sample_test() {
    assert_eq!(1242, run2("flqrgnkx".to_string()));
  }

  #[test]
  fn part1_test() {
    assert_eq!(8194, part1());
  }

  #[test]
  fn part2_test() {
    assert_eq!(1141, part2());
  }
}

pub fn part1() -> i32 {
    let input = include_str!("../inputs/day14.txt").trim();

    return run1(input.to_string());
}

pub fn part2() -> i32 {
    let input = include_str!("../inputs/day14.txt").trim();

    return run2(input.to_string());
}

fn run1(input:String) -> i32 {
    let grid:[[i32;128]; 128] = generate_grid(input);
    let mut squares_filled = 0;

    for i in 0..128 {
        for j in 0..128 {
            if grid[i][j] == 0 {
                squares_filled += 1;
            }
        }
    }

    return squares_filled;
}

fn run2(input:String) -> i32 {
    let mut grid:[[i32; 128]; 128] = generate_grid(input);
    let mut groupcount:i32 = 0;

    for i in 0..128 {
        for j in 0..128 {
            if grid[i][j] == 0 {
                groupcount += 1;
                fill_group(groupcount, &mut grid, i, j);
            }
        }
    }

    return groupcount;
}

fn generate_grid(input:String) -> [[i32; 128]; 128] {
    let hash = generate_empty_knothash();
    let mut grid:[[i32;128]; 128] = [[-1; 128];128];

    for i in 0..128 {
        let key = input.clone() + "-" + &i.to_string();
        let hash = knot_hash(key.to_string(), hash);
        for j in 0..16 {
            for k in 0..8 {
                if has_bit(hash[j], 7-k) {
                    grid[i][j*8+k] = 0;
                }
            }
        }
    }

    return grid;
}

fn has_bit(ch:u8, i:usize) -> bool{
    let two:u8 = 2;
    return (ch & two.pow(i as u32)) != 0;
}

fn generate_empty_knothash() -> [u8; 256] {
    let mut hash: [u8; 256] = [0; 256];

    for i in 0..256 {
        hash[i] = i as u8;
    }

    return hash;
}

fn fill_group(groupcount:i32, mut grid:&mut [[i32;128];128], i:usize, j:usize) {
    grid[i][j] = groupcount;
    if i != 0 && grid[i-1][j] == 0 {
        fill_group(groupcount, &mut grid, i-1,j);
    }
    if j != 0 && grid[i][j-1] == 0 {
        fill_group(groupcount, &mut grid, i,j-1);
    }
    if i != 127 && grid[i+1][j] == 0 {
        fill_group(groupcount, &mut grid, i+1,j);
    }
    if j != 127 && grid[i][j+1] == 0 {
        fill_group(groupcount, &mut grid, i,j+1);
    }
}

fn hash_step(v: &mut [u8; 256], index:&mut u8, skipsize:&mut u8, len: u8) {
    let start = index.clone();
    for i in 0..(len / 2) {
        let a = start.wrapping_add(i);
        let b = start.wrapping_add(len).wrapping_sub(i).wrapping_sub(1);
        let tmp = v[a as usize];
        v[a as usize] = v[b as usize];
        v[b as usize] = tmp;
    }

    *index = index.wrapping_add(len.clone()).wrapping_add(*skipsize);
    *skipsize = skipsize.wrapping_add(1);
}

fn dense_hash(v: &[u8;256]) -> [u8; 16] {
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

fn knot_hash(input: String, mut list: [u8; 256]) -> [u8; 16] {
    let mut index:u8 = 0;
    let mut skipsize:u8 = 0;

    let mut bytes:Vec<u8> = input.into_bytes();
    bytes.push(17);
    bytes.push(31);
    bytes.push(73);
    bytes.push(47);
    bytes.push(23);

    for _i in 0..64 {
        for len in &bytes {
            hash_step(&mut list, &mut index, &mut skipsize, *len);
        }
    }

    return dense_hash(&list);
}