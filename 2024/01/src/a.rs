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
  let mut result = 0;
  for i in 0..locations_left.len() {
    let dist = (locations_left[i] - locations_right[i]).abs();
    result += dist;
  }
  result
}
