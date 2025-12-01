use std::collections::HashSet;
use std::io;
use std::io::Read;

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    let mut grid = Vec::new();

    for line in input.lines() {
        let mut gridline = Vec::new();
        for c in line.chars() {
            gridline.push(c);
        }
        grid.push(gridline);
    }

    let mut tiles_seen = HashSet::new();
    let mut answer_p1 = 0;
    let mut answer_p2 = 0;

    for (y, row) in grid.iter().enumerate() {
        for (x, _) in row.iter().enumerate() {
            if tiles_seen.contains(&(y, x)) {
                continue;
            }

            let tiles_of_plot = discover_plot(&grid, y, x);

            let area = tiles_of_plot.len();
            let mut perimiter = 0;

            for tile in &tiles_of_plot {
                perimiter += perimiters_for_tile(&grid, tile).len();
                tiles_seen.insert(*tile);
            }

            let sides = count_sides_of_plot(&grid, &tiles_of_plot);

            let price = area * perimiter;
            let discount_price = area * sides;

            answer_p1 += price;
            answer_p2 += discount_price;
        }
    }

    println!("Part 1: {}", answer_p1);
    println!("Part 2: {}", answer_p2);
    Ok(())
}

fn count_sides_of_plot(grid: &Vec<Vec<char>>, tiles_of_plot: &HashSet<(usize, usize)>) -> usize {
    let mut sides = 0;
    let mut tile_perim_had: HashSet<((usize, usize), (usize, usize))> = HashSet::new();
    for &tile in tiles_of_plot {
        let c = grid[tile.0][tile.1];
        let perimiters = perimiters_for_tile(&grid, &tile);

        if perimiters.len() == 0 { continue; }
        for perimiter in perimiters {
            if tile_perim_had.contains(&(tile, perimiter)) { continue; }

            sides += 1;

            if tile.0 != perimiter.0 {
                 for i in 0..10000 {
                     if (tile.1+i) < grid[0].len() && !eq_crop(grid, &(perimiter.0, tile.1+i), c) && grid[tile.0][tile.1+i] == c {
                         tile_perim_had.insert(((tile.0, tile.1+i), (perimiter.0, tile.1+i)));
                     } else {
                         break;
                     }
                 }
                for i in 1..10000 {
                    if (tile.1+1-i) > 0 && !eq_crop(grid, &(perimiter.0, tile.1-i), c) && grid[tile.0][tile.1-i] == c {
                        tile_perim_had.insert(((tile.0, tile.1-i), (perimiter.0, tile.1-i)));
                    } else {
                        break;
                    }
                }
            } else if tile.1 != perimiter.1 {
                for i in 0..10000 {
                    if (tile.0+i) < grid.len() && !eq_crop(grid, &(tile.0+i, perimiter.1), c) && grid[tile.0+i][tile.1] == c {
                        tile_perim_had.insert(((tile.0+i, tile.1), (tile.0+i, perimiter.1)));
                    } else {
                        break;
                    }
                }
                for i in 1..10000 {
                    if (tile.0+1-i) > 0 && !eq_crop(grid, &(tile.0-i, perimiter.1), c) && grid[tile.0-i][tile.1] == c {
                        tile_perim_had.insert(((tile.0-i, tile.1), (tile.0-i, perimiter.1)));
                    } else {
                        break;
                    }
                }
            }
        }
    }

    sides
}

fn neighbors(pos: &(usize, usize)) -> [(usize, usize); 4] {
    [
        (pos.0 + 1, pos.1),
        (pos.0.wrapping_sub(1), pos.1),
        (pos.0, pos.1 + 1),
        (pos.0, pos.1.wrapping_sub(1)),
    ]
}

fn discover_plot(grid: &Vec<Vec<char>>, y: usize, x: usize) -> HashSet<(usize, usize)> {
    let mut result = HashSet::new();
    let mut stack = vec![(y, x)];

    let c = grid[y][x];

    while let Some(top) = stack.pop() {
        result.insert(top);

        neighbors(&top)
            .into_iter()
            .filter(|newpos| eq_crop(grid, &newpos, c) && !result.contains(newpos))
            .for_each(|newpos| stack.push(newpos));
    }

    result
}

fn perimiters_for_tile(grid: &Vec<Vec<char>>, pos: &(usize, usize)) -> Vec<(usize, usize)> {
    neighbors(pos)
        .into_iter()
        .filter(|newpos| !eq_crop(grid, newpos, grid[pos.0][pos.1]))
        .collect()
}

fn eq_crop(grid: &Vec<Vec<char>>, neighbour: &(usize, usize), c: char) -> bool {
    if neighbour.0 >= grid.len() || neighbour.1 >= grid[0].len() {
        return false;
    }

    grid[neighbour.0][neighbour.1] == c
}