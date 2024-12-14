use std::fs;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct Point {
  x: i32,
  y: i32,
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct Robot {
  p: Point,
  v: Point,
}

impl Robot {
  fn tick(&mut self, times: i32, wrapping: Point) {
    self.p.x = (self.p.x + self.v.x * times) % wrapping.x;
    if self.p.x < 0 {
      self.p.x += wrapping.x
    };
    self.p.y = (self.p.y + self.v.y * times) % wrapping.y;
    if self.p.y < 0 {
      self.p.y += wrapping.y
    };
  }

  fn quadrant(&self, wrapping: Point) -> Option<u8> {
    let middle_x = wrapping.x / 2;
    let middle_y = wrapping.y / 2;
    if self.p.x < middle_x && self.p.y < middle_y {
      Some(0)
    } else if self.p.x > middle_x && self.p.y < middle_y {
      Some(1)
    } else if self.p.x < middle_x && self.p.y > middle_y {
      Some(2)
    } else if self.p.x > middle_x && self.p.y > middle_y {
      Some(3)
    } else {
      None
    }
  }
}

fn parse(input: &str) -> Vec<Robot> {
  input
    .lines()
    .map(|line| -> Robot {
      let numbers: Vec<i32> = line
        .split(|c: char| !c.is_ascii_digit() && c != '-')
        .filter_map(|n| str::parse(n).ok())
        .collect();
      Robot {
        p: Point {
          x: numbers[0],
          y: numbers[1],
        },
        v: Point {
          x: numbers[2],
          y: numbers[3],
        },
      }
    })
    .collect()
}

fn solve(input: &str, wrapping: Point) -> u32 {
  parse(input)
    .iter_mut()
    .filter_map(|robot| {
      robot.tick(100, wrapping);
      robot.quadrant(wrapping)
    })
    .fold([0_u32; 4], |mut acc, quadrant| {
      acc[quadrant as usize] += 1;
      acc
    })
    .iter()
    .product()
}

pub fn main() -> u32 {
  let input = fs::read_to_string("input/in").unwrap();
  solve(input.trim(), Point { x: 101, y: 103 })
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_parse() {
    assert_eq!(
      parse("p=0,4 v=3,-3"),
      vec!(Robot {
        p: Point { x: 0, y: 4 },
        v: Point { x: 3, y: -3 },
      })
    )
  }

  const EXAMPLE_1: &str = "\
p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3";

  #[test]
  fn test_solve_example_1() {
    assert_eq!(solve(EXAMPLE_1, Point { x: 11, y: 7 }), 12)
  }
}
