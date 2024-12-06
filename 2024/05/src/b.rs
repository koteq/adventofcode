use std::collections::HashMap;
use std::{array, fs};

struct Puzzle {
  rules: Vec<(u8, u8)>,
  updates: Vec<Vec<u8>>,
}

impl Puzzle {
  fn new(rules_input: String, updates_input: String) -> Self {
    Self {
      rules: Puzzle::parse_rules(rules_input),
      updates: Puzzle::parse_updates(updates_input),
    }
  }

  fn parse_rules(input: String) -> Vec<(u8, u8)> {
    input
      .split("\n")
      .map(|line| {
        let [left, right] = line
          .split("|")
          .map(|number| number.parse::<u8>().unwrap())
          .collect::<Vec<u8>>()[..]
        else {
          panic!()
        };
        (left, right)
      })
      .collect()
  }

  fn parse_updates(input: String) -> Vec<Vec<u8>> {
    input
      .split("\n")
      .map(|line| line.split(",").map(|page| page.parse().unwrap()).collect())
      .collect()
  }

  fn solve(&mut  self) -> u32 {
    let mut result: u32 = 0;
    for pages in &mut self.updates {
      let mut map: HashMap<u8, usize> = HashMap::new();
      for (pos, page) in pages.iter().enumerate() {
        map.insert(*page, pos);
      }
      let valid = self.rules.iter().all(|(left, right)| {
        let (Some(pos_left), Some(pos_right)) = (map.get(&left), map.get(&right)) else {
          return true;
        };
        if pos_left < pos_right {
          return true;
        }
        false
      });
      if !valid {
        let mut i = 0;
        while i < self.rules.len() {
          let (left, right) = self.rules[i];
          if let (Some(pos_left), Some(pos_right)) = (map.get(&left).cloned(), map.get(&right).cloned()) {
            if pos_left > pos_right {
              pages.swap(pos_left, pos_right);
              map.insert(left, pos_right);
              map.insert(right, pos_left);
              i = 0;
              continue;
            }
          }
          i += 1;
        }
        let middle = pages[(pages.len() - 1) / 2];
        result += middle as u32;
      }
    }
    result
  }
}

pub fn main() -> u32 {
  let input = fs::read_to_string("input/in").unwrap().trim().to_string();
  let [rules_input, updates_input] = array::from_fn({
    let mut split = input.split("\n\n");
    move |_| String::from(split.next().unwrap())
  });

  let mut puzzle = Puzzle::new(rules_input, updates_input);
  puzzle.solve()
}
