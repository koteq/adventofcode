use std::collections::HashMap;

fn blink(memory: &mut HashMap<usize, HashMap<usize, usize>>, stone: usize, times: usize) -> usize {
  if times == 0 {
    1
  } else if let Some(count) = memory.get(&stone).and_then(|h| h.get(&times)) {
    count.to_owned()
  } else {
    let count: usize;
    if stone == 0 {
      count = blink(memory, 1, times - 1);
    } else {
      let str = stone.to_string();
      let is_even = str.len() % 2 == 0;
      if is_even {
        let (left, right) = str.split_at(str.len() / 2);
        count = blink(memory, left.parse().unwrap(), times - 1) + blink(memory, right.parse().unwrap(), times - 1)
      } else {
        count = blink(memory, stone * 2024, times - 1);
      }
    }
    memory.entry(stone).or_default().insert(times, count);
    count
  }
}

fn solve(input: &str, times: usize) -> usize {
  let mut result: usize = 0;
  let mut memory: HashMap<usize, HashMap<usize, usize>> = HashMap::new();
  let stones: Vec<usize> = input.split(' ').map(|n| n.parse().unwrap()).collect();
  for stone in stones {
    result += blink(&mut memory, stone, times)
  }
  result
}

pub fn main() -> usize {
  solve("7568 155731 0 972 1 6919238 80646 22", 75)
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_solve_example_1() {
    assert_eq!(solve("125 17", 6), 22);
  }

  #[test]
  fn test_solve_example_2() {
    assert_eq!(solve("125 17", 25), 55312);
  }
}