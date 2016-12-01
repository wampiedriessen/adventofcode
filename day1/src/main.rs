#[derive(Copy, Clone)]
struct Point {
    x: i32,
    y: i32,
}

impl std::cmp::PartialEq for Point {
    fn eq(&self, other: &Point) -> bool {
        self.x == other.x && self.y == other.y
    }
}

fn print_loc(p: Point) {
    println!("Y: {}", p.y);
    println!("X: {}", p.x);
    println!("Distance: {}", (p.y+p.x).abs() );
}

fn main() {
    println!("Input:");

    let mut input = String::new();

    std::io::stdin().read_line(&mut input)
        .expect("Failed to read line");

    let mut p = Point { x: 0, y: 0};

    let mut saved = Vec::new();

    let mut dir = 0; // 0 is north, 1 is east....

    let mut double: bool = false;

    saved.push(p);

    for s in input.split(", ") {
        let mut steps: i32;
        if s.contains("L") {
            let tmp: Vec<&str> = s.split("L").collect();
            steps = tmp[1].trim().parse().expect("NAN?");
            dir = (dir + 3) % 4;
        }
        else if s.contains("R") {
            let tmp: Vec<&str> = s.split("R").collect();
            steps = tmp[1].trim().parse().expect("NAN?");   
            dir = (dir + 1) % 4;
        } else {
            panic!("WHUTS!! {}", s);
        }

        while steps > 0 {
        
            match dir {
                0 => p.y += 1,
                1 => p.x += 1,
                2 => p.y -= 1,
                3 => p.x -= 1,
                _ => panic!("NO WAY!!"),
            }

            // println!("Command: {}", s);
            if !double && saved.contains(&p) {
                println!("Second time on this location:");
                print_loc(p);
                double = true;
            } else {
                saved.push(p);
            }

            steps -= 1;
        }

    }

    println!("Ends in:");
    print_loc(p);
}
