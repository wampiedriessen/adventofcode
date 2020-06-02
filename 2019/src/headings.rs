pub type Heading = i64;

pub const NORTH: Heading = 1;
pub const SOUTH: Heading = 2;
pub const WEST: Heading = 3;
pub const EAST: Heading = 4;

pub fn update_location(heading: Heading, x: i32, y: i32) -> (i32, i32) {
    match heading {
        NORTH => (x, y+1),
        SOUTH => (x, y-1),
        WEST => (x+1, y),
        EAST => (x-1, y),
        _ => panic!("Unknown Heading!")
    }
}

pub fn reverse_heading(heading: Heading) -> Result<Heading, &'static str> {
    match heading {
        NORTH => Ok(SOUTH),
        SOUTH => Ok(NORTH),
        WEST => Ok(EAST),
        EAST => Ok(WEST),
        _ => Err("Unknown Heading!")
    }
}
