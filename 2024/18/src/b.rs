use itertools::Itertools;
use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::fs;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct Point {
  x: i32,
  y: i32,
}

impl Point {
  fn new(x: i32, y: i32) -> Self {
    Self { x, y }
  }

  fn from_str(input: &str) -> Self {
    let mut split = input.split(',');
    Self::new(
      split.next().unwrap().parse().unwrap(),
      split.next().unwrap().parse().unwrap(),
    )
  }

  fn adjacent(self) -> [Self; 4] {
    [
      Self::new(self.x, self.y - 1),
      Self::new(self.x + 1, self.y),
      Self::new(self.x, self.y + 1),
      Self::new(self.x - 1, self.y),
    ]
  }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct State {
  position: Point,
  steps: usize,
}

impl State {
  fn new(position: Point, steps: usize) -> Self {
    Self { position, steps }
  }
}

impl Ord for State {
  fn cmp(&self, other: &Self) -> Ordering {
    // ordering is flipped to construct a min-heap
    other.steps.cmp(&self.steps)
  }
}

impl PartialOrd for State {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

// Dijkstra's algorithm
// https://doc.rust-lang.org/std/collections/binary_heap/index.html
fn solve(input: &str, take: usize, space: u8) -> Option<usize> {
  let start = Point::new(0, 0);
  let goal = Point::new(space.into(), space.into());
  let corrupted: HashSet<Point> = input.lines().take(take).map(Point::from_str).collect();

  let mut heap: BinaryHeap<State> = BinaryHeap::new();
  let mut dist: HashMap<Point, usize> =
    (0..=space)
      .cartesian_product(0..=space)
      .fold(HashMap::new(), |mut acc, (x, y)| {
        acc.insert(Point::new(x.into(), y.into()), usize::MAX);
        acc
      });
  heap.push(State::new(start, 0));
  dist.insert(start, 0);

  while let Some(State { position, steps }) = heap.pop() {
    if position == goal {
      return Some(steps);
    }
    if steps > *dist.get(&position).unwrap() {
      continue;
    }
    for adj in position.adjacent() {
      if corrupted.contains(&adj) {
        continue;
      }
      if adj.x < 0 || adj.y < 0 || adj.x > space.into() || adj.y > space.into() {
        continue;
      }
      let next = State::new(adj, steps + 1);
      if next.steps < *dist.get(&next.position).unwrap() {
        heap.push(next);
        dist.insert(adj, next.steps);
      }
    }
  }

  None
}

pub fn main() {
  let input = fs::read_to_string("input/in").unwrap();
  for i in 1024.. {
    if solve(input.trim(), i, 70).is_none() {
      println!("{}", input.lines().nth(i - 1).unwrap());
      break;
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_solve_example_1() {
    let input = include_str!("../input/s1");
    assert_eq!(solve(input.trim(), 12, 6), Some(22));
  }
}
