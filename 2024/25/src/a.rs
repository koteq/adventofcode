use itertools::Itertools;
use std::fs;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct Schematic {
  heights: [u8; 5],
}

impl Schematic {
  fn new(input: &str) -> Self {
    let bytes = input.as_bytes();
    let mut heights: [u8; 5] = [0; 5];
    for x in 0..5 {
      for y in 1..6 {
        if bytes[6 * y + x] == b'#' {
          heights[x] += 1;
        }
      }
    }
    Self { heights }
  }

  fn overlaps(self, other: Self) -> bool {
    self
      .heights
      .iter()
      .zip(other.heights)
      .any(|(a, b)| a + b > 5)
  }
}

fn parse(input: &str) -> (Vec<Schematic>, Vec<Schematic>) {
  let (keys_input, locks_input): (Vec<&str>, Vec<&str>) =
    input.split("\n\n").partition(|s| s.starts_with("....."));
  (
    keys_input.into_iter().map(Schematic::new).collect(),
    locks_input.into_iter().map(Schematic::new).collect(),
  )
}

fn solve(input: &str) -> usize {
  let (keys, locks) = parse(input);
  keys
    .into_iter()
    .cartesian_product(locks)
    .filter(|(key, lock)| !key.overlaps(lock.to_owned()))
    .count()
}

pub fn main() -> usize {
  let input = fs::read_to_string("input/in").unwrap();
  solve(input.trim())
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_solve_example_1() {
    let input = "\
#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####";
    assert_eq!(solve(input), 3);
  }
}
