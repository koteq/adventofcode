use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::fs;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Point {
  x: i32,
  y: i32,
}

impl Point {
  fn new(x: i32, y: i32) -> Self {
    Self { x, y }
  }

  fn from_usize(x: usize, y: usize) -> Option<Self> {
    Some(Self {
      x: i32::try_from(x).ok()?,
      y: i32::try_from(y).ok()?,
    })
  }

  fn adjacent(self) -> [Self; 4] {
    [
      Self::new(self.x, self.y - 1),
      Self::new(self.x + 1, self.y),
      Self::new(self.x, self.y + 1),
      Self::new(self.x - 1, self.y),
    ]
  }

  fn cheat(self) -> [Self; 4] {
    [
      Self::new(self.x, self.y - 2),
      Self::new(self.x + 2, self.y),
      Self::new(self.x, self.y + 2),
      Self::new(self.x - 2, self.y),
    ]
  }
}

fn parse(input: &str) -> (Point, Point, HashSet<Point>) {
  let mut start: Option<Point> = None;
  let mut end: Option<Point> = None;
  let track: HashSet<Point> =
    input
      .lines()
      .enumerate()
      .fold(HashSet::new(), |mut acc, (y, line)| {
        line.bytes().enumerate().for_each(|(x, b)| match b {
          b'S' => {
            start = Point::from_usize(x, y);
            acc.insert(Point::from_usize(x, y).unwrap());
          }
          b'E' => {
            end = Point::from_usize(x, y);
            acc.insert(Point::from_usize(x, y).unwrap());
          }
          b'.' => {
            acc.insert(Point::from_usize(x, y).unwrap());
          }
          _ => {}
        });
        acc
      });

  (start.unwrap(), end.unwrap(), track)
}

fn shortest_path(start: Point, track: &HashSet<Point>) -> HashMap<Point, u32> {
  let mut dist_map: HashMap<Point, u32> = HashMap::new();
  dist_map.insert(start, 0);
  let mut heap: BinaryHeap<Reverse<(u32, Point)>> = BinaryHeap::new();
  heap.push(Reverse((0, start)));
  while let Some(Reverse((dist, pos))) = heap.pop() {
    if dist_map.get(&pos).is_some_and(|&d| d < dist) {
      continue;
    }
    for adj in pos.adjacent() {
      if track.contains(&adj) && dist_map.get(&adj).is_none_or(|&d| d > dist + 1) {
        heap.push(Reverse((dist + 1, adj)));
        dist_map.insert(adj, dist + 1);
      }
    }
  }
  dist_map
}

fn solve(input: &str) -> usize {
  let (start, end, track) = parse(input);
  let dist_from_start: HashMap<Point, u32> = shortest_path(start, &track);
  let dist_from_end: HashMap<Point, u32> = shortest_path(end, &track);
  let fastest = dist_from_start.get(&end).unwrap().to_owned();
  let mut saves = 0;
  for (point, dist_to_start) in dist_from_start {
    for cheat_point in point.cheat() {
      if let Some(&dist_to_end) = dist_from_end.get(&cheat_point) {
        if let Some(save) = fastest.checked_sub(dist_to_end + dist_to_start + 2) {
          if save >= 100 {
            saves += 1;
          }
        }
      }
    }
  }
  saves
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
###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############";
    assert_eq!(solve(input), 14 + 14 + 2 + 4 + 2 + 3 + 5);
  }
}
