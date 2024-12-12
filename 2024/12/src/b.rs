use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;
use std::num::TryFromIntError;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
enum Direction {
  North,
  East,
  South,
  West,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct Point {
  x: i32,
  y: i32,
}

impl Point {
  fn new(x: i32, y: i32) -> Self {
    Self { x, y }
  }

  fn try_from(x: usize, y: usize) -> Result<Self, TryFromIntError> {
    Ok(Self {
      x: i32::try_from(x)?,
      y: i32::try_from(y)?,
    })
  }

  fn adjacent(self) -> [(Self, Direction); 4] {
    [
      (Self::new(self.x, self.y - 1), Direction::North),
      (Self::new(self.x + 1, self.y), Direction::East),
      (Self::new(self.x, self.y + 1), Direction::South),
      (Self::new(self.x - 1, self.y), Direction::West),
    ]
  }
}

fn parse_grid(input: &str) -> HashMap<Point, u8> {
  input
    .lines()
    .enumerate()
    .fold(HashMap::new(), |mut acc, (y, line)| {
      line.bytes().enumerate().for_each(|(x, c)| {
        acc.insert(Point::try_from(x, y).unwrap(), c);
      });
      acc
    })
}

fn calculate_gaps(vec: &mut Vec<i32>) -> usize {
  vec.sort_unstable();
  vec
    .windows(2)
    .map(|w| usize::try_from((w[1] - w[0] - 1).min(1)).unwrap())
    .sum()
}

fn calculate_sides(fence: &Vec<(Point, Direction)>) -> usize {
  let mut ordinate: HashMap<(i32, Direction), Vec<i32>> = HashMap::new();
  for (point, direction) in fence {
    let (key, value) = match direction {
      Direction::North | Direction::South => (point.y, point.x),
      Direction::East | Direction::West => (point.x, point.y),
    };
    ordinate
      .entry((key, *direction))
      .or_default()
      .push(value);
  }
  ordinate
    .values_mut()
    .map(calculate_gaps)
    .map(|g| g + 1)
    .sum()
}

fn solve(input: &str) -> usize {
  let mut price: usize = 0;
  let grid = parse_grid(input);
  let mut explored: HashSet<Point> = HashSet::new();

  let mut breadth_first_explore = |start_point: &Point, start_plant: &u8| -> (usize, usize) {
    if explored.contains(start_point) {
      return (0, 0);
    }
    let mut area = 1;
    let mut queue: VecDeque<Point> = VecDeque::new();
    let mut fence: Vec<(Point, Direction)> = Vec::new();
    explored.insert(*start_point);
    queue.push_back(*start_point);
    while let Some(point) = queue.pop_front() {
      for (adjacent, direction) in point.adjacent() {
        if let Some(plant) = grid.get(&adjacent) {
          if plant == start_plant {
            if explored.insert(adjacent) {
              queue.push_back(adjacent);
              area += 1;
            }
          } else {
            fence.push((point, direction));
          }
        } else {
          fence.push((point, direction));
        }
      }
    }
    let sides = calculate_sides(&fence);
    (area, sides)
  };

  for (point, plant) in &grid {
    let (area, sides) = breadth_first_explore(point, plant);
    price += area * sides;
  }
  price
}

pub fn main() -> usize {
  let input = fs::read_to_string("input/in").unwrap();
  solve(input.trim())
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_solve_example_0() {
    let input = "\
AAA";
    assert_eq!(solve(input), 12);
  }

  #[test]
  fn test_solve_example_1() {
    let input = "\
AAAA
BBCD
BBCC
EEEC";
    assert_eq!(solve(input), 80);
  }

  #[test]
  fn test_solve_example_2() {
    let input = "\
OOOOO
OXOXO
OOOOO
OXOXO
OOOOO";
    assert_eq!(solve(input), 436);
  }

  #[test]
  fn test_solve_example_3() {
    let input = "\
EEEEE
EXXXX
EEEEE
EXXXX
EEEEE";
    assert_eq!(solve(input), 236);
  }

  #[test]
  fn test_solve_example_4() {
    let input = "\
AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA";
    assert_eq!(solve(input), 368);
  }

  #[test]
  fn test_solve_example_5() {
    let input = "\
RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE";
    assert_eq!(solve(input), 1206);
  }
}
