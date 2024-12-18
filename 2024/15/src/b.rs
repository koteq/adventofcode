use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter};
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
  BoxL,
  BoxR,
  Wall,
  // Empty,
}

struct Warehouse {
  map: HashMap<Point, Tile>,
  map_size: Point,
  robot: Point,
}

impl Debug for Warehouse {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    let mut buff: Vec<u8> = Vec::new();
    for y in 0..self.map_size.y {
      let mut line: Vec<u8> = Vec::with_capacity(usize::try_from(self.map_size.x).unwrap());
      for x in 0..self.map_size.x {
        let point = Point::new(x, y);
        line.push(match self.map.get(&point) {
          Some(Tile::Wall) => b'#',
          Some(Tile::BoxL) => b'[',
          Some(Tile::BoxR) => b']',
          _ => {
            if point == self.robot {
              b'@'
            } else {
              b'.'
            }
          }
        });
      }
      buff.extend(line);
      buff.push(b'\n');
    }
    write!(f, "{}", String::from_utf8(buff).unwrap())
  }
}

impl Warehouse {
  fn new(map_str: &str) -> Self {
    let mut robot: Option<Point> = None;
    let mut w: i32 = 0;
    let mut h: i32 = 0;
    let map: HashMap<Point, Tile> =
      map_str
        .lines()
        .enumerate()
        .fold(HashMap::new(), |mut acc, (y, line)| {
          line.bytes().enumerate().for_each(|(x, b)| {
            match b {
              b'@' => robot = Some(Point::from_usize(x * 2, y).unwrap()),
              b'O' => {
                acc.insert(Point::from_usize(x * 2, y).unwrap(), Tile::BoxL);
                acc.insert(Point::from_usize(x * 2 + 1, y).unwrap(), Tile::BoxR);
              }
              b'#' => {
                acc.insert(Point::from_usize(x * 2, y).unwrap(), Tile::Wall);
                acc.insert(Point::from_usize(x * 2 + 1, y).unwrap(), Tile::Wall);
              }
              _ => {}
            }
            w = w.max(i32::try_from(x).unwrap() * 2 + 2);
            h = h.max(i32::try_from(y + 1).unwrap());
          });
          acc
        });
    Self {
      map,
      map_size: Point::new(w, h),
      robot: robot.unwrap(),
    }
  }

  fn operate(&mut self, moves: &[u8]) {
    'move_loop: for movement in moves.iter().copied() {
      let mut boxes_stack: Vec<Point> = Vec::new();
      let mut force_stack: Vec<Point> = Vec::from_iter([self.robot]);
      // println!("{self:?}");
      // println!("Move {}:", movement as char);
      // look for a free space or a wall, skipping movable boxes
      while let Some(initial_pos) = force_stack.pop() {
        let mut current_pos = initial_pos;
        loop {
          match self.map.get(&current_pos) {
            Some(Tile::Wall) => continue 'move_loop,
            Some(&tile @ (Tile::BoxL | Tile::BoxR)) => {
              boxes_stack.push(current_pos);
              if current_pos != initial_pos && (movement == b'^' || movement == b'v') {
                if tile == Tile::BoxL {
                  force_stack.push(current_pos + b'>');
                } else {
                  force_stack.push(current_pos + b'<');
                }
              }
            }
            None => {
              if current_pos != initial_pos {
                break;
              }
            }
          }
          current_pos += movement;
        }
      }
      // move boxes if any
      let mut moved: HashSet<Point> = HashSet::new();
      while let Some(point) = boxes_stack.pop() {
        if moved.contains(&point) {
          continue;
        }
        if let Some(tile) = self.map.remove(&point) {
          self.map.insert(point + movement, tile);
          moved.insert(point);
        }
      }
      // move robot
      self.robot += movement;
    }
    // println!("{self:?}");
  }

  fn gps_sum(&self) -> usize {
    self
      .map
      .iter()
      .filter(|(_point, &tile)| tile == Tile::BoxL)
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
  fn test_solve_example_0() {
    let input = "\
#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^";
    assert_eq!(solve(input), 0);
  }

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
    assert_eq!(solve(input), 0);
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
    assert_eq!(solve(input), 9021);
  }
}
