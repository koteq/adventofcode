use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;
use std::num::TryFromIntError;

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

  fn adjacent(self) -> [Self; 4] {
    [
      Self::new(self.x, self.y - 1),
      Self::new(self.x + 1, self.y),
      Self::new(self.x, self.y + 1),
      Self::new(self.x - 1, self.y),
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

fn solve(input: &str) -> usize {
  let mut price: usize = 0;
  let grid = parse_grid(input);
  let mut explored: HashSet<Point> = HashSet::new();
  
  let mut breadth_first_explore = |start_point: &Point, start_plant: &u8| -> (usize, usize) {
    if explored.contains(start_point) {
      return (0, 0);
    }
    let mut area = 1;
    let mut perimeter = 0;
    let mut queue: VecDeque<Point> = VecDeque::new();
    explored.insert(*start_point);
    queue.push_back(*start_point);
    while let Some(point) = queue.pop_front() {
      for adjacent in point.adjacent() {
        if let Some(plant) = grid.get(&adjacent) {
          if plant == start_plant {
            if explored.insert(adjacent) {
              queue.push_back(adjacent);
              area += 1;
            }
          } else {
            perimeter += 1;
          }
        } else {
          perimeter += 1;
        }
      }
    }
    (area, perimeter)
  };
  
  for (point, plant) in &grid {
    let (area, perimeter) = breadth_first_explore(point, plant);
    price += area * perimeter;
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
X";
    assert_eq!(solve(input), 4);
  }

  #[test]
  fn test_solve_example_1() {
    let input = "\
AAAA
BBCD
BBCC
EEEC";
    assert_eq!(solve(input), 140);
  }

  #[test]
  fn test_solve_example_2() {
    let input = "\
OOOOO
OXOXO
OOOOO
OXOXO
OOOOO";
    assert_eq!(solve(input), 772);
  }

  #[test]
  fn test_solve_example_3() {
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
    assert_eq!(solve(input), 1930);
  }
}
