use std::fs;

fn parse(input: &str) -> Vec<u64> {
  input
    .split('\n')
    .map(|s| s.parse::<u64>().unwrap())
    .collect()
}

const fn evolve(mut number: u64) -> u64 {
  number = (number ^ (number * 64)) % 16_777_216;
  number = (number ^ (number / 32)) % 16_777_216;
  number = (number ^ (number * 2048)) % 16_777_216;
  number
}

fn solve(input: &str, ticks: usize) -> u64 {
  let mut numbers = parse(input);
  for i in 0..numbers.len() {
    for _ in 0..ticks {
      numbers[i] = evolve(numbers[i]);
    }
  }
  numbers.iter().sum()
}

pub fn main() -> u64 {
  let input = fs::read_to_string("input/in").unwrap();
  solve(input.trim(), 2_000)
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_solve_example_0() {
    let input = "\
123";
    solve(input, 10);
  }

  #[test]
  fn test_solve_example_1() {
    let input = "\
1
10
100
2024";
    assert_eq!(solve(input, 2_000), 37_327_623);
  }
}
