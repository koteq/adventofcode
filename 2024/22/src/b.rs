use std::collections::HashMap;
use std::fs;

fn parse(input: &str) -> Vec<Vec<u64>> {
  input
    .split('\n')
    .map(|s| Vec::from_iter([s.parse::<u64>().unwrap()]))
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
  let mut seq: HashMap<(i8, i8, i8, i8), Vec<Option<u8>>> = HashMap::new();
  for t in 0..ticks {
    for i in 0..numbers.len() {
      let next = evolve(numbers[i][t]);
      numbers[i].push(next);
      if t >= 3 {
        let key = (
          i8::try_from(numbers[i][t - 2] % 10).unwrap() - i8::try_from(numbers[i][t - 3] % 10).unwrap(),
          i8::try_from(numbers[i][t - 1] % 10).unwrap() - i8::try_from(numbers[i][t - 2] % 10).unwrap(),
          i8::try_from(numbers[i][t - 0] % 10).unwrap() - i8::try_from(numbers[i][t - 1] % 10).unwrap(),
          i8::try_from(numbers[i][t + 1] % 10).unwrap() - i8::try_from(numbers[i][t - 0] % 10).unwrap(),
        );
        if !seq.contains_key(&key) {
          seq.insert(key, vec![None; numbers.len()]);
        }
        let value = seq.get_mut(&key).unwrap();
        if value[i].is_none() {
          value[i] = Some(u8::try_from(numbers[i][t + 1] % 10).unwrap());
        }
      }
    }
  }
  seq
    .values()
    .map(|v| v.iter().filter_map(|&v| v).map(u64::from).sum())
    .max()
    .unwrap()
}

pub fn main() -> u64 {
  let input = fs::read_to_string("input/in").unwrap();
  solve(input.trim(), 2_000)
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_solve_example_1() {
    let input = "\
1
2
3
2024";
    assert_eq!(solve(input, 2_000), 23);
  }
}
