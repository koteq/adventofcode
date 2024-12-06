use std::collections::HashSet;
use std::fs;

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
  visited: HashSet<Point>,
}

impl Guard {
  fn new(position: Point) -> Self {
    Self {
      visited: HashSet::from([position.clone()]),
      position,
      direction: Direction::North,
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

  fn move_next(&mut self, grid: &Grid) -> Result<(), ()> {
    let next_position = self.get_next_position();
    match grid.get(&next_position) {
      Some('#') => {
        self.direction = self.direction.turn_right();
        Ok(())
      }
      Some('.') => {
        self.visited.insert(next_position.clone());
        self.position = next_position;
        Ok(())
      }
      _ => Err(()),
    }
  }
}

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

  fn insert(&mut self, point: Point, value: char) {
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
      Point {
        x: i16::try_from(x).unwrap(),
        y: i16::try_from(y).unwrap(),
      },
      '.',
    );
    Guard::new(Point {
      x: x.try_into().unwrap(),
      y: y.try_into().unwrap(),
    })
  }

  fn solve(&mut self) -> usize {
    while let Ok(_) = self.guard.move_next(&self.grid) {}
    self.guard.visited.len()
  }
}

pub fn main() -> usize {
  let input = fs::read_to_string("input/in").unwrap().trim().to_string();
  let mut puzzle = Puzzle::new(&input);
  puzzle.solve()
}
