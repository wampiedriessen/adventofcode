#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part1_sample_test() {
        assert_eq!(0, run1(1));
        assert_eq!(3, run1(12));
        assert_eq!(2, run1(23));
        assert_eq!(31, run1(1024));
    }

    #[test]
    fn part2_sample_test() {
        assert_eq!(806, run2(805));
        assert_eq!(362, run2(358));
        assert_eq!(304, run2(150));
    }

    #[test]
    fn part1_test() {
        assert_eq!(326, part1());
    }

    #[test]
    fn part2_test() {
        assert_eq!(363010, part2());
    }
}

pub fn part1() -> i32 {
    let input = include_str!("../inputs/day03.txt").parse::<i32>().unwrap();
    return run1(input);
}

pub fn part2() -> i32 {
    let input = include_str!("../inputs/day03.txt").parse::<i32>().unwrap();
    return run2(input);
}

fn run1(input: i32) -> i32 {
    if input == 1 { return 0; }
    
    let base = get_lowest_odd_root(input);
    let steps = (base + 1) / 2;
    let base_sq = base.pow(2);

    let increment = (input - base_sq) / (2 * steps);
    return steps + (input - base_sq - (2 * increment + 1) * steps).abs();
}

fn run2(input: i32) -> i32 {
    let width = 600;
    let height = 600;
    // Base 1d array
    let mut grid_raw = vec![0; width * height];

    // Vector of 'width' elements slices
    let mut grid_base: Vec<_> = grid_raw.as_mut_slice().chunks_mut(width).collect();

    // Final 2d array
    let grid: &mut [&mut [_]] = grid_base.as_mut_slice();

    let mut x = 301;
    let mut y = 299;
    grid[300][300] = 1;
    grid[301][300] = 1;
    grid[301][299] = 2;
    let mut smallestx = 299;
    let mut smallesty = 298;
    let mut biggestx = 302;
    let mut biggesty = 301;
    loop {
        while x > smallestx {
            x -= 1;
            if check_last(input, grid, x,y) {
                return grid[x][y];
            }
        }
        smallestx -= 1;
        while y < biggesty {
            y += 1;
            if check_last(input, grid, x,y) {
                return grid[x][y];
            }
        }
        biggesty += 1;

        while x < biggestx {
            x += 1;
            if check_last(input, grid, x,y) {
                return grid[x][y];
            }
        }
        biggestx += 1;
        while y > smallesty {
            y -= 1;
            if check_last(input, grid, x,y) {
                return grid[x][y];
            }
        }
        smallesty -= 1;
    }
}

fn get_lowest_odd_root(input:i32) -> i32 {
    return (((input as f32).sqrt() + 1.0) / 2.0) as i32 * 2 - 1;
}

fn check_last(input:i32, grid: &mut [&mut [i32]], x: usize, y:usize) -> bool
{
    grid[x][y] = 
        grid[x-1][y-1] +
        grid[x][y-1] +
        grid[x+1][y-1] +
        grid[x-1][y] +
        grid[x+1][y] +
        grid[x-1][y+1] +
        grid[x][y+1] +
        grid[x+1][y+1];

    return grid[x][y] > input;
}