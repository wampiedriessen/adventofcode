fn endrace(r : i32, d : i32) {
    println!("Right: {}", r);
    println!("Down: {}", d);
    println!("Sum: {}", d+r);
}

fn main1() {
    let input = 361527;
    println!("input: {}", input);

    let base:i32 = (input as f32).sqrt() as i32;
    let kwadraat = base.pow(2);
    
    println!("base: {}", base);
    println!("kwadraat: {}", kwadraat);

    let mut stepsdown: i32 = (base-1) / 2;
    let mut stepsright: i32 = (base-1) / 2;

    let mut diff = input - kwadraat;
    let mut dir = 0; // 0 is up, 1 is left, etc.
    while diff > 0 {
        diff -= base;
        dir = (dir + 1) % 4;
    }
    dir = (dir + 1) % 4;
    diff += base;
    if diff == 0 { // we eindigen op een hoekpunt
        endrace(stepsright, stepsdown);
    }
    if (dir % 2) == 0 {
        // we are up or down
        stepsdown -= diff;
    }
    if (dir % 2) == 1 {
        // we are left or right
        stepsright -= diff;
    }
    endrace(stepsright.abs(), stepsdown.abs());
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

    println!("x: {}, y: {}, val: {}", x,y,grid[x][y]);

    if grid[x][y] > input {
        println!("DONE! {}", grid[x][y]);
        return true;
    }
    return false;
}

fn main() {
    main1();
    let input = 361527;
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
                return;
            }
        }
        smallestx -= 1;
        while y < biggesty {
            y += 1;
            if check_last(input, grid, x,y) {
                return;
            }
        }
        biggesty += 1;

        while x < biggestx {
            x += 1;
            if check_last(input, grid, x,y) {
                return;
            }
        }
        biggestx += 1;
        while y > smallesty {
            y -= 1;
            if check_last(input, grid, x,y) {
                return;
            }
        }
        smallesty -= 1;
    }
}