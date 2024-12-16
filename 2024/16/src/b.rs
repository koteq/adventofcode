use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::fs;
use std::num::TryFromIntError;
use std::ops::Add;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct Point {
  x: i32,
  y: i32,
}

impl Point {
  fn new(x: i32, y: i32) -> Self {
    Self { x, y }
  }

  fn from_usize(x: usize, y: usize) -> Result<Self, TryFromIntError> {
    Ok(Self {
      x: i32::try_from(x)?,
      y: i32::try_from(y)?,
    })
  }
}

impl Add<Direction> for Point {
  type Output = Self;

  fn add(self, direction: Direction) -> Self::Output {
    match direction {
      Direction::North => Self::new(self.x, self.y - 1),
      Direction::East => Self::new(self.x + 1, self.y),
      Direction::South => Self::new(self.x, self.y + 1),
      Direction::West => Self::new(self.x - 1, self.y),
    }
  }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum Direction {
  North,
  East,
  South,
  West,
}

impl Direction {
  const ALL: [Self; 4] = [Self::North, Self::East, Self::South, Self::West];

  fn next(self) -> Self {
    match self {
      Self::North => Self::East,
      Self::East => Self::South,
      Self::South => Self::West,
      Self::West => Self::North,
    }
  }

  fn next_back(self) -> Self {
    match self {
      Self::North => Self::West,
      Self::East => Self::North,
      Self::South => Self::East,
      Self::West => Self::South,
    }
  }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct State {
  score: u32,
  position: Point,
  direction: Direction,
}

impl Ord for State {
  fn cmp(&self, other: &Self) -> Ordering {
    // flip order to implement min-heap queue
    other.score.cmp(&self.score)
  }
}

impl PartialOrd for State {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

// Dijkstra's shortest path algorithm.
// see https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
// see https://doc.rust-lang.org/std/collections/binary_heap/index.html
fn lowest_score_path(start: Point, end: Point, maze: &HashSet<Point>) -> usize {
  let mut prev: HashMap<(Point, Direction), HashSet<(Point, Direction)>> = HashMap::new();
  let mut heap: BinaryHeap<State> = BinaryHeap::new();
  let mut lowest_score: HashMap<(Point, Direction), u32> =
    maze.iter().fold(HashMap::new(), |mut acc, &point| {
      for direction in Direction::ALL {
        acc.insert((point, direction), u32::MAX);
      }
      acc
    });

  heap.push(State {
    score: 0,
    position: start,
    direction: Direction::East,
  });
  lowest_score.insert((start, Direction::East), 0);

  while let Some(State {
    score,
    position,
    direction,
  }) = heap.pop()
  {
    // prepare next states accessible from the current state
    let mut next_states = vec![
      State {
        score: score + 1_000,
        position,
        direction: direction.next(),
      },
      State {
        score: score + 1_000,
        position,
        direction: direction.next_back(),
      },
    ];

    // add forward movement if there is a floor in front
    let forward = position + direction;
    if maze.contains(&forward) {
      next_states.push(State {
        score: score + 1,
        position: forward,
        direction,
      });
    }

    for next_state in next_states {
      let lowest = lowest_score
        .get(&(next_state.position, next_state.direction))
        .unwrap()
        .to_owned();
      if lowest >= score {
        // found a better path
        heap.push(next_state);
        lowest_score.insert(
          (next_state.position, next_state.direction),
          next_state.score,
        );
        prev
          .entry((next_state.position, next_state.direction))
          .or_default()
          .insert((position, direction));
      }
    }
  }
  depth_first_explore_count(end, &prev)
}

fn depth_first_explore_count(
  end: Point,
  prev: &HashMap<(Point, Direction), HashSet<(Point, Direction)>>,
) -> usize {
  let mut stack: Vec<(Point, Direction)> = Vec::from_iter(Direction::ALL.map(|d| (end, d)));
  let mut visited: HashSet<(Point, Direction)> = HashSet::new();
  while let Some(vertex) = stack.pop() {
    if visited.insert(vertex) {
      if let Some(prev_vertices) = prev.get(&vertex) {
        stack.extend(prev_vertices);
      }
    }
  }
  let visited_points: HashSet<Point> = visited.iter().map(|&(point, _direction)| point).collect();
  visited_points.len()
}

fn debug(visited: &HashSet<Point>, maze: &HashSet<Point>) {
  for y in 0..=16 {
    for x in 0..=16 {
      let point = Point::new(x, y);
      if visited.contains(&point) {
        print!("O");
      } else if maze.contains(&point) {
        print!(".");
      } else {
        print!(" ");
      }
    }
    println!();
  }
}

fn parse(input: &str) -> (Point, Point, HashSet<Point>) {
  let mut start: Option<Point> = None;
  let mut end: Option<Point> = None;
  let maze =
    input
      .lines()
      .enumerate()
      .fold(HashSet::new(), |mut acc: HashSet<Point>, (y, line)| {
        line.bytes().enumerate().for_each(|(x, b)| {
          if b != b'#' {
            let point = Point::from_usize(x, y).unwrap();
            acc.insert(point);
            if b == b'S' {
              start = Some(point);
            }
            if b == b'E' {
              end = Some(point);
            }
          }
        });
        acc
      });
  (start.unwrap(), end.unwrap(), maze)
}

fn solve(input: &str) -> usize {
  let (start, end, maze) = parse(input);
  lowest_score_path(start, end, &maze)
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
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############";
    assert_eq!(solve(input), 45);
  }

  #[test]
  fn test_solve_example_2() {
    let input = "\
#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################";
    assert_eq!(solve(input), 64);
  }
}
