use std::{cmp::max, collections::HashMap, fs};

#[derive(Clone, Eq, PartialEq, Hash)]
struct Vec2 {
    x: i32,
    y: i32,
}

impl Vec2 {
    const NORTH: Vec2 = Vec2 { x: 0, y: -1 };
    const SOUTH: Vec2 = Vec2 { x: 0, y: 1 };
    const WEST: Vec2 = Vec2 { x: -1, y: 0 };
    const EAST: Vec2 = Vec2 { x: 1, y: 0 };
    const NORTH_EAST: Vec2 = Vec2 { x: 1, y: -1 };
    const NORTH_WEST: Vec2 = Vec2 { x: -1, y: -1 };
    const SOUTH_EAST: Vec2 = Vec2 { x: 1, y: 1 };
    const SOUTH_WEST: Vec2 = Vec2 { x: -1, y: 1 };

    fn from_string(string: &str) -> Self {
        let (x, y) = string
            .split_once(",")
            .expect(format!("Unable to parse Vec2 from '{}'", string).as_str());
        Self {
            x: x.parse().expect(format!("Invalid number {}", x).as_str()),
            y: y.parse().expect(format!("Invalid number {}", y).as_str()),
        }
    }

    fn add(&self, other: &Vec2) -> Vec2 {
        Vec2 {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }

    fn sub(&self, other: &Vec2) -> Vec2 {
        self.add(&Vec2 {
            x: -other.x,
            y: -other.y,
        })
    }

    fn direction(&self) -> Vec2 {
        if self.x == 0 && self.y < 0 {
            return Vec2::NORTH;
        }
        if self.x == 0 && self.y > 0 {
            return Vec2::SOUTH;
        }
        if self.x < 0 && self.y == 0 {
            return Vec2::WEST;
        }
        if self.x > 0 && self.y == 0 {
            return Vec2::EAST;
        }
        if self.x > 0 && self.y < 0 {
            return Vec2::NORTH_EAST;
        }
        if self.x < 0 && self.y < 0 {
            return Vec2::NORTH_WEST;
        }
        if self.x > 0 && self.y > 0 {
            return Vec2::SOUTH_EAST;
        }
        if self.x < 0 && self.y > 0 {
            return Vec2::SOUTH_WEST;
        }
        unreachable!();
    }
}

#[derive(Clone)]
struct Line {
    start: Vec2,
    end: Vec2,
    point: Vec2,
    direction: Vec2,
}

impl Line {
    fn from_string(string: &str) -> Self {
        let (coord1, coord2) = string
            .split_once(" -> ")
            .expect(format!("Unable to parse Line from '{}'", string).as_str());
        let start = Vec2::from_string(coord1);
        let end = Vec2::from_string(coord2);
        Self {
            start: start.clone(),
            end: end.clone(),
            point: start.clone(),
            direction: end.clone().sub(&start.clone()).direction(),
        }
    }

    fn is_diagonal(&self) -> bool {
        !(self.start.x == self.end.x || self.start.y == self.end.y)
    }
}

impl Iterator for Line {
    type Item = Vec2;

    fn next(&mut self) -> Option<Vec2> {
        // check if gone beyond end
        if self.point == self.end.add(&self.direction) {
            return None;
        }

        let current_point = self.point.clone();
        self.point = self.point.add(&self.direction);
        Some(current_point)
    }
}

fn get_lines() -> Vec<Line> {
    fs::read_to_string("input")
        .expect("Unable to open input file")
        .lines()
        .map(Line::from_string)
        .collect()
}

fn main() {
    let lines = get_lines();

    // horizontal and vertical only

    let horizontal_vertical_lines: Vec<&Line> = lines.iter().filter(|l| !l.is_diagonal()).collect();
    let mut grid: HashMap<Vec2, u32> = HashMap::new();

    for line in horizontal_vertical_lines {
        for coord in line.clone().into_iter() {
            let count = grid.entry(coord).or_insert(0);
            *count += 1;
        }
    }

    let count = grid.iter().filter(|(_x, y)| **y > 1).count();

    println!("Part 1: {}", count);

    // all directions only

    let mut grid: HashMap<Vec2, u32> = HashMap::new();

    for line in lines {
        for coord in line.clone().into_iter() {
            let count = grid.entry(coord).or_insert(0);
            *count += 1;
        }
    }

    let count = grid.iter().filter(|(_x, y)| **y > 1).count();

    println!("Part 2: {}", count);
}
