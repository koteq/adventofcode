use itertools::Itertools;
use std::collections::{HashMap, HashSet};
use std::{fs, ops};

#[derive(Debug, Eq, PartialEq, Hash)]
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
    let grid = Grid::new(input);
    let mut antennas_map: HashMap<char, Vec<Point>> = HashMap::new();
    Self::init_antennas(&grid, &mut antennas_map);
    
    let mut antinodes_set: HashSet<Point> = HashSet::new();
    for (_frequency, antennas) in antennas_map {
      for (antenna_a, antenna_b) in antennas.iter().tuple_combinations() {
        let (antinode_a, antinode_b) = Self::find_antinodes(antenna_a, antenna_b);
        if grid.contains(&antinode_a) {
          antinodes_set.insert(antinode_a);
        }
        if grid.contains(&antinode_b) {
          antinodes_set.insert(antinode_b);
        }
      }
    }
    antinodes_set.len()
  }

  fn init_antennas(grid: &Grid, antennas_map: &mut HashMap<char, Vec<Point>>) {
    for (point, frequency) in grid {
      if *frequency != '.' {
        antennas_map.entry(*frequency).or_default().push(point);
      }
    }
  }

  fn find_antinodes(a: &Point, b: &Point) -> (Point, Point) {
    let diff = a - b;
    (a + &diff, b - &diff)
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
  fn test_get_antinodes() {
    let antinodes = Puzzle::find_antinodes(&Point::new(4, 3), &Point::new(5, 5));
    assert_eq!(antinodes, (Point::new(3, 1), Point::new(6, 7)))
  }
}
