use std::collections::HashSet;
use std::fs;

fn parse(input: &str) -> (Vec<&str>, Vec<&str>) {
  let mut split = input.split("\n\n");
  (
    split.next().unwrap().split(", ").collect(),
    split.next().unwrap().split('\n').collect(),
  )
}

fn is_possible(design: &str, patterns: &[&str]) -> bool {
  let mut stack: Vec<usize> = vec![0];
  let mut visited: HashSet<usize> = HashSet::new();
  while let Some(pos) = stack.pop() {
    for &pat in patterns {
      let end = pos + pat.len();
      if end > design.len() {
        continue;
      }
      if &design[pos..end] == pat {
        if end == design.len() {
          return true;
        }
        if visited.insert(end) {
          stack.push(end);
        }
      }
    }
  }
  false
}

fn solve(input: &str) -> usize {
  let (patterns, designs) = parse(input);
  designs
    .iter()
    .filter(|&&design| is_possible(design, &patterns))
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
r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb";
    assert_eq!(solve(input), 6);
  }
}
