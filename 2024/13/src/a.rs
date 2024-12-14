use regex::Regex;
use std::fs;

#[derive(Debug)]
struct Machine {
  buttons: [[u32; 2]; 2],
  prize: [u32; 2],
}

impl Machine {
  const A_COST: u32 = 3;
  const B_COST: u32 = 1;
  const MAX_PRESSES: u32 = 100;

  fn bruteforce(&self) -> Option<(u32, u32)> {
    for a in 0..=Self::MAX_PRESSES {
      for b in 0..=Self::MAX_PRESSES {
        if self.prize[0]
          == a * self.buttons[0][0]
          + b * self.buttons[1][0] && self.prize[1]
          == a * self.buttons[0][1]
          + b * self.buttons[1][1] {
          return Some((a, b));
        }
      }
    }
    None
  }
}

fn parse(input: &str) -> Vec<Machine> {
  let re = Regex::new(r"Button A: X\+(?<ax>\d+), Y\+(?<ay>\d+)\nButton B: X\+(?<bx>\d+), Y\+(?<by>\d+)\nPrize: X=(?<px>\d+), Y=(?<py>\d+)").unwrap();
  re.captures_iter(input)
    .map(|c| Machine {
      buttons: [
        [c["ax"].parse().unwrap(), c["ay"].parse().unwrap()],
        [c["bx"].parse().unwrap(), c["by"].parse().unwrap()],
      ],
      prize: [c["px"].parse().unwrap(), c["py"].parse().unwrap()],
    })
    .collect()
}

fn solve(input: &str) -> u32 {
  let machines = parse(input);
  machines
    .iter()
    .filter_map(Machine::bruteforce)
    .map(|(a_presses, b_presses)| a_presses * Machine::A_COST + b_presses * Machine::B_COST)
    .sum()
}

pub fn main() -> u32 {
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
    assert_eq!(machine.bruteforce(), Some((80, 40)));
  }

  #[test]
  fn test_machine_2() {
    let machine = &parse(EXAMPLE_1)[1];
    assert_eq!(machine.bruteforce(), None);
  }

  #[test]
  fn test_machine_3() {
    let machine = &parse(EXAMPLE_1)[2];
    assert_eq!(machine.bruteforce(), Some((38, 86)));
  }

  #[test]
  fn test_machine_4() {
    let machine = &parse(EXAMPLE_1)[3];
    assert_eq!(machine.bruteforce(), None);
  }

  #[test]
  fn test_solve_example_1() {
    assert_eq!(solve(EXAMPLE_1), 480);
  }
}
