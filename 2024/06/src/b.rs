use std::collections::HashSet;
use std::fs;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
enum Direction {
  North,
  East,
  South,
  West,
}

impl Direction {
  fn turn_right(&self) -> Self {
    use Direction::*;
    match *self {
      North => East,
      East => South,
      South => West,
      West => North,
    }
  }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
struct Point {
  x: i16,
  y: i16,
}

struct Guard {
  direction: Direction,
  position: Point,
}

enum MoveOrTurn {
  Move {
    new_point: Point,
  },
  Turn {
    turn_point: Point,
    turn_direction: Direction,
  },
}

impl Guard {
  fn new(position: Point, direction: Direction) -> Self {
    Self {
      position,
      direction,
    }
  }

  fn get_next_position(&self) -> Point {
    use Direction::*;
    match self.direction {
      North => Point {
        x: self.position.x,
        y: self.position.y - 1,
      },
      East => Point {
        x: self.position.x + 1,
        y: self.position.y,
      },
      South => Point {
        x: self.position.x,
        y: self.position.y + 1,
      },
      West => Point {
        x: self.position.x - 1,
        y: self.position.y,
      },
    }
  }

  fn move_next(&mut self, grid: &Grid) -> Result<MoveOrTurn, ()> {
    let next_position = self.get_next_position();
    use MoveOrTurn::*;
    match grid.get(&next_position) {
      Some('#') => {
        self.direction = self.direction.turn_right();
        Ok(Turn {
          turn_point: self.position.clone(),
          turn_direction: self.direction.clone(),
        })
      }
      Some('.') => {
        self.position = next_position;
        Ok(Move {
          new_point: self.position.clone(),
        })
      }
      _ => Err(()),
    }
  }
}

#[derive(Clone)]
struct Grid {
  data: Vec<Vec<char>>,
}

impl Grid {
  fn new(input: &String) -> Self {
    Grid {
      data: input
        .split("\n")
        .map(|line| line.chars().collect())
        .collect::<Vec<Vec<char>>>(),
    }
  }

  fn insert(&mut self, point: &Point, value: char) {
    self.data[usize::try_from(point.y).unwrap()][usize::try_from(point.x).unwrap()] = value
  }

  fn get(&self, point: &Point) -> Option<&char> {
    let (Ok(x), Ok(y)) = (usize::try_from(point.x), usize::try_from(point.y)) else {
      return None;
    };
    self.data.get(y).and_then(|line| line.get(x))
  }
}

struct Puzzle {
  grid: Grid,
  guard: Guard,
}

impl Puzzle {
  fn new(input: &String) -> Self {
    let mut grid = Grid::new(input);
    let guard = Puzzle::init_guard(&mut grid);
    Self { grid, guard }
  }

  fn init_guard(grid: &mut Grid) -> Guard {
    let mut x: usize = 0;
    let Some(y) =
      grid
        .data
        .iter()
        .position(|line| match line.iter().position(|char| *char == '^') {
          Some(i) => {
            x = i;
            true
          }
          _ => false,
        })
    else {
      panic!()
    };
    grid.insert(
      &Point {
        x: i16::try_from(x).unwrap(),
        y: i16::try_from(y).unwrap(),
      },
      '.',
    );
    Guard::new(
      Point {
        x: x.try_into().unwrap(),
        y: y.try_into().unwrap(),
      },
      Direction::North,
    )
  }

  fn solve(&mut self) -> usize {
    let start = self.guard.position.clone();
    let mut grid_copy = self.grid.clone();
    let mut obstacle_positions: HashSet<Point> = HashSet::new();
    use MoveOrTurn::*;
    while let Ok(action) = self.guard.move_next(&self.grid) {
      match action {
        Move { new_point } => {
          // walk modified puzzle to check if it loops
          if obstacle_positions.contains(&new_point) {
            continue;
          }
          let mut turns: HashSet<(Point, Direction)> = HashSet::new();
          let mut looping_guard = Guard::new(start.clone(), Direction::North);
          grid_copy.insert(&new_point, '#');
          while let Ok(action) = looping_guard.move_next(&grid_copy) {
            match action {
              Turn {
                turn_point,
                turn_direction,
              } => {
                let turn = (turn_point.clone(), turn_direction.clone());
                if turns.contains(&turn) {
                  obstacle_positions.insert(new_point.clone());
                  break;
                }
                turns.insert(turn);
              }
              _ => {}
            }
          }
          grid_copy.insert(&new_point, '.');
        }
        _ => {}
      }
    }

    obstacle_positions.len()
  }
}

pub fn main() -> usize {
  let input = fs::read_to_string("input/in").unwrap().trim().to_string();
  let mut puzzle = Puzzle::new(&input);
  puzzle.solve()
}
