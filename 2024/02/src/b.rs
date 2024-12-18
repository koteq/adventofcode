use std::fs;

pub fn main() -> usize {
  let input = fs::read_to_string("input/in").unwrap();
  let result = input
    .trim()
    .lines()
    .map(|line| {
      line
        .split(" ")
        .map(|number| number.parse::<i32>().unwrap())
        .collect::<Vec<i32>>()
    })
    .filter(|levels| {
      if (all_increasing(levels) || all_decreasing(levels)) && all_safe(levels) {
        return true;
      }
      for i in 0..levels.len() {
        let mut tolerate = levels.clone();
        tolerate.remove(i);
        if (all_increasing(&tolerate) || all_decreasing(&tolerate)) && all_safe(&tolerate) {
          return true;
        }
      }
      false
    })
    .count();
  result
}

fn all_increasing(vector: &Vec<i32>) -> bool {
  let mut previous = i32::MIN;
  for &i in vector {
    if i <= previous {
      return false;
    }
    previous = i;
  }
  true
}

fn all_decreasing(vector: &Vec<i32>) -> bool {
  let mut previous = i32::MAX;
  for &i in vector {
    if i >= previous {
      return false;
    }
    previous = i;
  }
  true
}

fn all_safe(vector: &[i32]) -> bool {
  let min = 1;
  let max = 3;
  for i in 1..vector.len() {
    let a = vector[i - 1];
    let b = vector[i];
    let diff = (a - b).abs();
    if diff < min || diff > max {
      return false;
    }
  }
  true
}
