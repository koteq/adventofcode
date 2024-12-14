use regex::Regex;
use std::fs;

#[derive(Debug, Copy, Clone)]
struct Point {
  x: f64,
  y: f64,
}

#[derive(Debug)]
struct Machine {
  button_a: Point,
  button_b: Point,
  prize: Point,
}

impl Machine {
  const A_COST: u64 = 3;
  const B_COST: u64 = 1;
  const PRIZE_OFFSET: f64 = 10_000_000_000_000.0;

  fn solve(&self) -> Option<(u64, u64)> {
    // solve a system of two linear equations:
    // y = {A.Y} / {A.X} * x
    // y = {B.Y} / {B.X} * x + (P.Y - P.X * B.Y / B.X)
    // where first line passes through point 0,0 and first button x,y
    // and the second line passes through the prize point and second button x,y (negative from prize point)
    let x = (self.prize.y - self.prize.x * self.button_b.y / self.button_b.x)
      / (self.button_a.y / self.button_a.x - self.button_b.y / self.button_b.x);
    let _y = self.button_a.y / self.button_a.x * x;

    let a_presses = (x / self.button_a.x).round(); // == y / self.button_a.y
    let b_presses = ((self.prize.x - x) / self.button_b.x).round(); // == (self.prize.y - y)/self.button_b.y

    if self.prize.x == a_presses * self.button_a.x + b_presses * self.button_b.x
      && self.prize.y == a_presses * self.button_a.y + b_presses * self.button_b.y
    {
      return Some((a_presses as u64, b_presses as u64));
    }
    None
  }
}

fn parse(input: &str) -> Vec<Machine> {
  let re = Regex::new(r"Button A: X\+(?<ax>\d+), Y\+(?<ay>\d+)\nButton B: X\+(?<bx>\d+), Y\+(?<by>\d+)\nPrize: X=(?<px>\d+), Y=(?<py>\d+)").unwrap();
  re.captures_iter(input)
    .map(|c| Machine {
      button_a: Point {
        x: c["ax"].parse().unwrap(),
        y: c["ay"].parse().unwrap(),
      },
      button_b: Point {
        x: c["bx"].parse().unwrap(),
        y: c["by"].parse().unwrap(),
      },
      prize: Point {
        x: c["px"].parse::<f64>().unwrap() + Machine::PRIZE_OFFSET,
        y: c["py"].parse::<f64>().unwrap() + Machine::PRIZE_OFFSET,
      },
    })
    .collect()
}

fn solve(input: &str) -> u64 {
  let machines = parse(input);
  machines
    .iter()
    .filter_map(Machine::solve)
    .map(|(a_presses, b_presses)| a_presses * Machine::A_COST + b_presses * Machine::B_COST)
    .sum()
}

pub fn main() -> u64 {
  let input = fs::read_to_string("input/in").unwrap();
  solve(input.trim())
}

#[cfg(test)]
mod tests {
  use super::*;

  const EXAMPLE_1: &str = "\
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279";

  #[test]
  fn test_machine_1() {
    let machine = &parse(EXAMPLE_1)[0];
    assert_eq!(machine.solve(), Some((80, 40)));
  }

  #[test]
  fn test_machine_2() {
    let machine = &parse(EXAMPLE_1)[1];
    assert_eq!(machine.solve(), None);
  }

  #[test]
  fn test_machine_3() {
    let machine = &parse(EXAMPLE_1)[2];
    assert_eq!(machine.solve(), Some((38, 86)));
  }

  #[test]
  fn test_machine_4() {
    let machine = &parse(EXAMPLE_1)[3];
    assert_eq!(machine.solve(), None);
  }

  #[test]
  fn test_solve_example_1() {
    assert_eq!(solve(EXAMPLE_1), 480);
  }
}
