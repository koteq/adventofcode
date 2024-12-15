use std::collections::HashMap;
use std::fs;
use std::num::TryFromIntError;
use std::ops::{Add, AddAssign, Sub, SubAssign};

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

impl Add<u8> for Point {
  type Output = Self;

  fn add(self, movement: u8) -> Self {
    match movement {
      b'^' => Self::new(self.x, self.y - 1),
      b'>' => Self::new(self.x + 1, self.y),
      b'v' => Self::new(self.x, self.y + 1),
      b'<' => Self::new(self.x - 1, self.y),
      _ => unreachable!(),
    }
  }
}

impl Sub<u8> for Point {
  type Output = Self;

  fn sub(self, movement: u8) -> Self {
    match movement {
      b'v' => Self::new(self.x, self.y - 1),
      b'<' => Self::new(self.x + 1, self.y),
      b'^' => Self::new(self.x, self.y + 1),
      b'>' => Self::new(self.x - 1, self.y),
      _ => unreachable!(),
    }
  }
}

impl AddAssign<u8> for Point {
  fn add_assign(&mut self, movement: u8) {
    *self = *self + movement;
  }
}

impl SubAssign<u8> for Point {
  fn sub_assign(&mut self, movement: u8) {
    *self = *self - movement;
  }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum Tile {
  Box,
  Wall,
  // Empty,
}

impl Tile {
  fn new(b: u8) -> Option<Self> {
    match b {
      b'O' => Some(Self::Box),
      b'#' => Some(Self::Wall),
      // b'.' => Some(Self::Empty),
      _ => None,
    }
  }
}

struct Warehouse {
  map: HashMap<Point, Tile>,
  robot: Point,
}

impl Warehouse {
  fn new(map_str: &str) -> Self {
    let mut robot: Option<Point> = None;
    let map: HashMap<Point, Tile> =
      map_str
        .lines()
        .enumerate()
        .fold(HashMap::new(), |mut acc, (y, line)| {
          line.bytes().enumerate().for_each(|(x, b)| {
            if b == b'@' {
              robot = Some(Point::from_usize(x, y).unwrap());
            } else if let Some(tile) = Tile::new(b) {
              acc.insert(Point::from_usize(x, y).unwrap(), tile);
            }
          });
          acc
        });
    Self {
      map,
      robot: robot.unwrap(),
    }
  }

  fn operate(&mut self, moves: &[u8]) {
    'move_loop: for movement in moves.iter().copied() {
      let initial_pos = self.robot;
      let mut current_pos = self.robot;
      // look for a free space or a wall, skipping movable boxes
      loop {
        current_pos += movement;
        match self.map.get(&current_pos) {
          Some(Tile::Wall) => continue 'move_loop,
          Some(Tile::Box) => {},
          None => break,
        }
      }
      // move boxes if any
      while current_pos != initial_pos {
        let previous_pos = current_pos;
        current_pos -= movement;
        if let Some(tile) = self.map.remove(&current_pos) {
          assert_eq!(tile, Tile::Box);
          self.map.insert(previous_pos, tile);
        }
      }
      // move robot
      self.robot += movement;
    }
  }

  fn gps_sum(&self) -> usize {
    self
      .map
      .iter()
      .filter(|(_point, &tile)| tile == Tile::Box)
      .map(|(&point, _tile)| {
        usize::try_from(point.y).unwrap() * 100 + usize::try_from(point.x).unwrap()
      })
      .sum()
  }
}

fn parse(input: &str) -> (Warehouse, Vec<u8>) {
  let mut split = input.split("\n\n");
  let warehouse = Warehouse::new(split.next().unwrap());
  let moves = split.next().unwrap().replace('\n', "").into_bytes();
  (warehouse, moves)
}

fn solve(input: &str) -> usize {
  let (mut warehouse, moves) = parse(input);
  warehouse.operate(moves.as_slice());
  warehouse.gps_sum()
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
########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<";
    assert_eq!(solve(input), 2028);
  }

  #[test]
  fn test_solve_example_2() {
    let input = "\
##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^";
    assert_eq!(solve(input), 10092);
  }
}
