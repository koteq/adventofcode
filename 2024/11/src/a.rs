struct Stones {
  data: Vec<usize>,
}

impl Stones {
  fn new(input: &str) -> Self {
    Self {
      data: input.split(' ').map(|n| n.parse().unwrap()).collect(),
    }
  }
  
  fn len(&self) -> usize {
    self.data.len()
  }
  
  fn blink(&mut self) {
    let mut i: usize = 0;
    while i < self.data.len() {
      let val = self.data[i];
      if val == 0 {
        self.data[i] = 1;
      } else {
        let str = val.to_string();
        let is_even = str.len() % 2 == 0;
        if is_even {
          let (left, right) = str.split_at(str.len() / 2);
          self.data[i] = left.parse().unwrap();
          self.data.insert(i, right.parse().unwrap());
          i += 1;
        } else {
          self.data[i] *= 2024
        }
      }
      i += 1;
    }
  }
}

fn solve(input: &str, blinks: usize) -> usize {
  let mut stones = Stones::new(input);
  for _ in 0..blinks {
    stones.blink()
  }
  stones.len()
}

pub fn main() -> usize {
  solve("7568 155731 0 972 1 6919238 80646 22", 25)
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