use std::collections::HashSet;
use std::fs;
use std::ops::{AddAssign, RemAssign};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct Point {
  x: i32,
  y: i32,
}

impl Point {
  const WRAPPING: Self = Point { x: 101, y: 103 };
}

impl AddAssign for Point {
  fn add_assign(&mut self, other: Self) {
    self.x += other.x;
    self.y += other.y;
  }
}

impl RemAssign for Point {
  fn rem_assign(&mut self, other: Self) {
    self.x %= other.x;
    self.y %= other.y;
    // who cares
    if self.x < 0 {
      self.x += other.x;
    };
    if self.y < 0 {
      self.y += other.y;
    };
  }
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct Robot {
  p: Point,
  v: Point,
  cache: Vec<Point>,
}

impl Robot {
  fn new(initial_point: Point, vector: Point) -> Self {
    let mut current_point = initial_point;
    let mut cache: Vec<Point> = Vec::new();
    // iterate all possible positions, checking if it outside a tree
    loop {
      cache.push(current_point);
      current_point += vector;
      current_point %= Point::WRAPPING;
      if current_point == initial_point {
        break;
      }
    }
    Self {
      p: initial_point,
      v: vector,
      cache,
    }
  }

  fn tick(&mut self, times: i32) {
    self.p.x = (self.p.x + self.v.x * times) % Point::WRAPPING.x;
    if self.p.x < 0 {
      self.p.x += Point::WRAPPING.x;
    };
    self.p.y = (self.p.y + self.v.y * times) % Point::WRAPPING.y;
    if self.p.y < 0 {
      self.p.y += Point::WRAPPING.y;
    };
  }
  
  fn next_pos(&self, tick: usize) -> Point {
    self.cache[tick % self.cache.len()]
  }
}

fn is_outside_tree(point: Point) -> bool {
  // roughly estimate if point is within a tree triangle
  // see https://adventofcode.com/2015 for visuals
  let mid_x = Point::WRAPPING.x / 2;
  (0..(mid_x - point.y)).contains(&point.x)
    || ((mid_x + 1 + point.y)..Point::WRAPPING.x).contains(&point.x)
}

fn debug(robots: &Vec<Robot>, tick: usize) {
  let mut result = String::new();
  let robot_pos: HashSet<Point> = robots.iter().map(|r| r.next_pos(tick)).collect();
  for y in 0..Point::WRAPPING.y {
    for x in 0..Point::WRAPPING.x {
      if robot_pos.contains(&Point { x, y }) {
        result.push('1'); 
        // result.push(char::from_digit(c as u32, 10).unwrap());
      } else {
        result.push('.');
      }
    }
    result.push('\n');
  }
  println!("{result}");
}

fn parse(input: &str) -> Vec<Robot> {
  input
    .lines()
    .map(|line| -> Robot {
      let numbers: Vec<i32> = line
        .split(|c: char| !c.is_ascii_digit() && c != '-')
        .filter_map(|n| str::parse(n).ok())
        .collect();
      let point = Point {
        x: numbers[0],
        y: numbers[1],
      };
      let vector = Point {
        x: numbers[2],
        y: numbers[3],
      };
      Robot::new(point, vector)
    })
    .collect()
}

fn solve(input: &str) {
  let mut tick = 0;
  let mut robots = parse(input);
  'outer: loop {
    let mut overlap: HashSet<Point> = HashSet::new();
    for robot in &robots {
      if !overlap.insert(robot.next_pos(tick)) {
        tick += 1;
        continue 'outer;
      }
    }
    debug(&robots, tick);
    println!("Tick: {tick}");
  }
}

pub fn main() {
  let input = fs::read_to_string("input/in").unwrap();
  solve(input.trim());
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test() {}
}
