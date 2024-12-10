use std::collections::HashMap;
use std::{fs, ops};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct Point {
  x: i32,
  y: i32,
}

impl Point {
  fn new(x: i32, y: i32) -> Self {
    Self { x, y }
  }
}

impl ops::Add<Direction> for Point {
  type Output = Self;

  fn add(self, direction: Direction) -> Self::Output {
    match direction {
      Direction(0) => Self::new(self.x, self.y - 1),
      Direction(1) => Self::new(self.x + 1, self.y),
      Direction(2) => Self::new(self.x, self.y + 1),
      Direction(3) => Self::new(self.x - 1, self.y),
      _ => panic!(),
    }
  }
}

impl ops::Sub<Direction> for Point {
  type Output = Self;

  fn sub(self, direction: Direction) -> Self::Output {
    match direction {
      Direction(0) => Self::new(self.x, self.y + 1),
      Direction(1) => Self::new(self.x - 1, self.y),
      Direction(2) => Self::new(self.x, self.y - 1),
      Direction(3) => Self::new(self.x + 1, self.y),
      _ => panic!(),
    }
  }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct Direction(u8);

impl Direction {
  const FIRST: Self = Self(0);
  const LAST: Self = Self(3);
  
  fn next(self) -> Option<Self> {
    match self.0 {
      0..4 => Some(Self(self.0 + 1)),
      _ => None
    }
  }
}

#[derive(Debug)]
struct DirectionRange {
  direction: Direction,
}

impl DirectionRange {
  fn from(direction: Direction) -> Self {
    Self { direction }
  }
}

impl Iterator for DirectionRange {
  type Item = Direction;

  fn next(&mut self) -> Option<Self::Item> {
    if self.direction.0 > Direction::LAST.0 {
      return None;
    }
    let result = Some(self.direction);
    self.direction.0 += 1;
    result
  }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct Height(u8);

impl Height {
  const MIN: Self = Self(0);
  const MAX: Self = Self(9);
}

#[derive(Debug, Copy, Clone)]
struct Rating(usize);

impl Rating {
  fn new() -> Self {
    Self(0)
  }

  fn increment(&mut self) {
    self.0 += 1;
  }
}

impl ops::AddAssign<Self> for Rating {
  fn add_assign(&mut self, rhs: Self) {
    self.0 += rhs.0;
  }
}

struct Grid {
  data: Vec<Vec<Height>>,
}

impl Grid {
  fn new(input: &str) -> Self {
    let data = input
      .lines()
      .map(|line| line.as_bytes().iter().map(|c| Height(c - b'0')).collect())
      .collect();
    Self { data }
  }

  fn get(&self, point: Point) -> Option<&Height> {
    self
      .data
      .get(usize::try_from(point.y).ok()?)?
      .get(usize::try_from(point.x).ok()?)
  }
}

impl<'a> IntoIterator for &'a Grid {
  type Item = (Point, Height);
  type IntoIter = GridIterator<'a>;

  fn into_iter(self) -> Self::IntoIter {
    GridIterator {
      point: Point::new(0, 0),
      grid: self,
    }
  }
}

struct GridIterator<'a> {
  point: Point,
  grid: &'a Grid,
}

impl Iterator for GridIterator<'_> {
  type Item = (Point, Height);

  fn next(&mut self) -> Option<Self::Item> {
    let value = self.grid.get(self.point)?.to_owned();
    let result = Some((self.point, value));
    self.point.x += 1;
    if self.point.x >= i32::try_from(self.grid.data[0].len()).unwrap() {
      self.point.x = 0;
      self.point.y += 1;
    }
    result
  }
}

struct Hiker<'a> {
  grid: &'a Grid,
}

impl<'a> Hiker<'a> {
  fn new(grid: &'a Grid) -> Self {
    Self { grid }
  }

  fn depth_first_explore(&self, start: Point) -> usize {
    let mut ratings: HashMap<Point, Rating> = HashMap::new();
    let mut current_pos = start;
    let mut current_dir = Direction::FIRST;
    let mut path: Vec<Direction> = Vec::new();
    loop {
      if let Some((adj_point, adj_direction, adj_height)) =
        self.next_adjacent_uphill(current_pos, DirectionRange::from(current_dir))
      {
        path.push(adj_direction);
        current_pos = adj_point;
        current_dir = Direction::FIRST;
        if adj_height == Height::MAX {
          ratings.entry(adj_point).or_insert(Rating::new()).increment();
        }
      } else if let Some(pop_direction) = path.pop() {
        if let Some(next_direction) = pop_direction.next() {
          current_pos = current_pos - pop_direction;
          current_dir = next_direction;
        }
      } else {
        break;
      }
    }
    ratings.values().map(|rating| rating.0).sum()
  }

  fn next_adjacent_uphill(
    &self,
    point: Point,
    direction_range: DirectionRange,
  ) -> Option<(Point, Direction, Height)> {
    let height = self.grid.get(point)?;
    for direction in direction_range {
      let adj_point = point + direction;
      if let Some(adj_height) = self.grid.get(adj_point) {
        if adj_height.0 == height.0 + 1 {
          return Some((adj_point, direction, *adj_height));
        }
      }
    }
    None
  }
}

pub fn solve(input: &str) -> usize {
  let mut result = 0;
  let grid = Grid::new(input);
  let hiker = Hiker::new(&grid);
  for (point, height) in &grid {
    if height == Height::MIN {
      result += hiker.depth_first_explore(point);
    }
  }
  result
}

pub fn main() -> usize {
  let input = fs::read_to_string("input/in").unwrap();
  solve(input.trim())
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_solve_example_1() {
    let input = "\
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732";
    assert_eq!(solve(input), 81);
  }
}
