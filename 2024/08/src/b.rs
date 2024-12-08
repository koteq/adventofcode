use itertools::Itertools;
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::{fmt, fs, ops};

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
struct Point {
  x: i32,
  y: i32,
}

impl Point {
  const fn new(x: i32, y: i32) -> Self {
    Self { x, y }
  }
}

impl ops::Add<&Point> for &Point {
  type Output = Point;

  fn add(self, other: &Point) -> Point {
    Point {
      x: self.x + other.x,
      y: self.y + other.y,
    }
  }
}

impl ops::Sub<&Point> for &Point {
  type Output = Point;

  fn sub(self, other: &Point) -> Point {
    Point {
      x: self.x - other.x,
      y: self.y - other.y,
    }
  }
}

impl ops::Mul<u8> for &Point {
  type Output = Point;

  fn mul(self, other: u8) -> Point {
    Point {
      x: self.x * i32::from(other),
      y: self.y * i32::from(other),
    }
  }
}

struct Grid {
  data: Vec<Vec<char>>,
}

impl Grid {
  fn new(input: &str) -> Self {
    let data = input.lines().map(|line| line.chars().collect()).collect();
    Self { data }
  }

  fn get(&self, point: &Point) -> Option<&char> {
    self
      .data
      .get(usize::try_from(point.y).ok()?)
      .and_then(|row| row.get(usize::try_from(point.x).ok()?))
  }

  fn contains(&self, point: &Point) -> bool {
    self.get(point).is_some()
  }

  fn insert(&mut self, point: &Point, value: char) {
    if self.contains(point) {
      self.data[usize::try_from(point.y).unwrap()][usize::try_from(point.x).unwrap()] = value;
    }
  }
}

impl Debug for Grid {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{}",
      self.data.iter().map(|row| row.iter().join("")).join("\n")
    )
  }
}

impl<'a> IntoIterator for &'a Grid {
  type Item = (Point, &'a char);
  type IntoIter = GridIterator<'a>;

  fn into_iter(self) -> Self::IntoIter {
    GridIterator {
      x: 0,
      y: 0,
      grid: self,
    }
  }
}

struct GridIterator<'a> {
  x: i32,
  y: i32,
  grid: &'a Grid,
}

impl<'a> Iterator for GridIterator<'a> {
  type Item = (Point, &'a char);

  fn next(&mut self) -> Option<Self::Item> {
    let point = Point::new(self.x, self.y);
    let value = self.grid.get(&point)?;
    self.x += 1;
    if self.x >= self.grid.data[0].len().try_into().unwrap() {
      self.x = 0;
      self.y += 1;
    }
    Some((point, value))
  }
}

struct Puzzle {}

impl Puzzle {
  fn solve(input: &str) -> usize {
    let mut grid = Grid::new(input);
    let mut antennas_map: HashMap<char, Vec<Point>> = HashMap::new();
    Self::init_antennas(&grid, &mut antennas_map);

    let mut antinodes_set: HashSet<Point> = HashSet::new();
    for (_frequency, antennas) in antennas_map {
      for (antenna_a, antenna_b) in antennas.iter().tuple_combinations() {
        antinodes_set.extend(Self::find_antinodes(&grid, antenna_a, antenna_b));
      }
    }

    for a in &antinodes_set {
      grid.insert(a, '#');
    }
    // println!("{grid:?}");

    antinodes_set.len()
  }

  fn init_antennas(grid: &Grid, antennas_map: &mut HashMap<char, Vec<Point>>) {
    for (point, frequency) in grid {
      if *frequency != '.' {
        antennas_map.entry(*frequency).or_default().push(point);
      }
    }
  }

  fn find_antinodes(grid: &Grid, a: &Point, b: &Point) -> Vec<Point> {
    let mut result: Vec<Point> = vec![a.clone(), b.clone()];
    let diff = a - b;
    for i in 1..u8::MAX {
      let antinode_a = a + &(&diff * i);
      let antinode_b = b - &(&diff * i);
      let contains_a = grid.contains(&antinode_a);
      let contains_b = grid.contains(&antinode_b);
      if contains_a {
        result.push(antinode_a);
      }
      if contains_b {
        result.push(antinode_b);
      }
      if !contains_a && !contains_b {
        break;
      }
    }
    result
  }
}

pub fn main() -> usize {
  let input = fs::read_to_string("input/in").unwrap();
  Puzzle::solve(input.trim())
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_solve() {
    Puzzle::solve(
      "\
T.........
...T......
.T........
..........
..........
..........
..........
..........
..........
..........",
    );
  }
}
