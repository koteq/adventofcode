use std::collections::HashMap;
use std::fs;

pub fn main() -> i32 {
  let mut locations_left: Vec<i32> = Vec::new();
  let mut locations_right: Vec<i32> = Vec::new();
  fs::read_to_string("input/in").unwrap().trim().lines()
    .for_each(|line| {
      let mut split = line.split("   ");
      locations_left.push(split.next().unwrap().parse::<i32>().unwrap());
      locations_right.push(split.next().unwrap().parse::<i32>().unwrap());
    });
  locations_left.sort_unstable();
  locations_right.sort_unstable();
  let mut right_map: HashMap<i32, i32> = HashMap::new();
  for n in &locations_right {
    right_map.entry(*n).and_modify(|e| *e += 1).or_insert(1);
  }
  let mut result = 0;
  for n in &locations_left {
    result += n * right_map.get(n).unwrap_or(&0);
  }
  result
}
