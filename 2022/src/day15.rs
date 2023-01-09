use std::collections::HashSet;
use std::str::FromStr;
use crate::Day;

pub struct Day15 {
    pub input: Vec<String>
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
struct Coord {
    x: i64,
    y: i64
}

impl Coord {
    fn mhd(&self, other: &Coord) -> i64 {
        (self.x - other.x).abs() +
        (self.y - other.y).abs()
    }
}

impl FromStr for Coord {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // String format:
        // x=2, y=18
        let spl: Vec<&str> = s.split(", ").collect();
        Ok(Coord {
            x: spl[0][2..].parse().unwrap(),
            y: spl[1][2..].parse().unwrap()
        })
    }
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
struct SensorData {
    sensor: Coord,
    beacon: Coord,
    range: i64
}

impl SensorData {
    fn new(sensor: Coord, beacon: Coord) -> Self {
        SensorData {
            range: sensor.mhd(&beacon),
            sensor,
            beacon
        }
    }
}

impl FromStr for SensorData {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let spl: Vec<&str> = s.split(':').collect();
        Ok(SensorData::new(
            spl[0][10..].parse().unwrap(),
            spl[1][22..].parse().unwrap()
        ))
    }
}

impl Day15 {
    fn check_level(&self, sensors: &mut Vec<SensorData>, level: i64, minx: i64, maxx: i64, break_at_hole: bool) -> (i64, i64) {
        // sort by most likely to cover x first
        sensors.sort_by(|a, b| {
            let arange = a.sensor.x - (a.range - (level - a.sensor.y).abs());
            let brange = b.sensor.x - (b.range - (level - b.sensor.y).abs());

            arange.cmp(&brange).then(a.cmp(b))
        });

        let mut covered_x_values: i64 = 0;
        let mut x_seen_until = minx;

        for sens in sensors {
            // we "move" sensor to our level, calculating it's leftover range
            let leftover_range = sens.range - (level - sens.sensor.y).abs();
            if leftover_range < 0 {
                // irrelevant sensor
                continue;
            }

            let leftcover = (sens.sensor.x-leftover_range).max(x_seen_until);
            let rightcover = (sens.sensor.x+leftover_range).min(maxx);

            if rightcover < x_seen_until { continue; }
            if break_at_hole && (leftcover != x_seen_until) { return (0, x_seen_until); }

            covered_x_values += rightcover - leftcover + 1;

            x_seen_until = rightcover + 1;
        }

        (covered_x_values, 0)
    }

    fn sensors(&self) -> Vec<SensorData> {
        self.input.iter().map(|x| x.parse().expect("line not in format")).collect()
    }

    fn part1_num_beacons_on_line(&self, sensors: &Vec<SensorData>, checklevel: i64) -> i64 {
        let mut seen = HashSet::new();
        for s in sensors {
            if s.beacon.y == checklevel {
                seen.insert(s.beacon.x);
            }
        }

        seen.len() as i64
    }

    fn part1_real(&self, checklevel: i64) -> i64 {
        let mut sensors = self.sensors();

        let (levelcoverage, _) = self.check_level(&mut sensors, checklevel, i64::MIN, i64::MAX-1, false);

        levelcoverage - self.part1_num_beacons_on_line(&sensors, checklevel)
    }

    fn part2_real(&self, min: i64, max: i64) -> i64 {
        let mut sensors = self.sensors();

        for level in min..=max {
            let (num, x) = self.check_level(&mut sensors, level, min, max, true);

            if num == 0 {
                return x*4000000+level;
            }
        }

        0
    }

}

impl Day for Day15 {
    fn part1(&self) -> String {
        self.part1_real(2000000).to_string()
    }

    fn part2(&self) -> String { self.part2_real(0, 4000000).to_string() }
}

#[cfg(test)]
mod test {
    use super::*;

    const EXAMPLE: &'static str = "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3";

    #[test]
    fn part1_example() {
        let d = Day15{ input: EXAMPLE.lines().map(|s| s.to_string()).collect() };
        assert_eq!(26, d.part1_real(10));
    }

    #[test]
    fn part1() {
        let d = Day15{ input: include_str!("../inputs/day15.txt").lines().map(|s| s.to_string()).collect() };
        assert_eq!("5125700", d.part1());
    }

    #[test]
    fn part2_example() {
        let d = Day15{ input: EXAMPLE.lines().map(|s| s.to_string()).collect() };
        assert_eq!(56000011, d.part2_real(0, 20));
    }

    #[test]
    fn part2() {
        let d = Day15{ input: include_str!("../inputs/day15.txt").lines().map(|s| s.to_string()).collect() };
        assert_eq!("11379394658764", d.part2());
    }
}