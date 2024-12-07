use std::collections::HashSet;
use std::fs;

fn is_solvable(target: u64, numbers: &Vec<u64>) -> bool {
  let mut results = HashSet::from([0]);
  for number in numbers {
    let mut new_results: HashSet<u64> = HashSet::new();
    for operator in ['+', '*'] {
      for result in &results {
        new_results.insert(match operator {
          '+' => result + number,
          '*' => result * number,
          _ => panic!(),
        });
      }
    }
    results = new_results;
  }
  results.contains(&target)
}

pub fn main() -> u64 {
  let mut result = 0;
  fs::read_to_string("input/in")
    .unwrap()
    .trim()
    .lines()
    .for_each(|line| {
      let mut split = line.split(": ");
      let target = split
        .next()
        .and_then(|s| s.parse::<u64>().ok())
        .unwrap();
      let numbers = split
        .next()
        .unwrap()
        .split(' ')
        .map(|n| n.parse::<u64>().unwrap())
        .collect::<Vec<u64>>();
      if is_solvable(target, &numbers) {
        result += target;
      }
    });
  result
}
