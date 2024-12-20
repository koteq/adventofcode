use std::collections::{HashMap, HashSet};
use std::fs;

fn parse(input: &str) -> (Vec<&str>, Vec<&str>) {
  let mut split = input.split("\n\n");
  (
    split.next().unwrap().split(", ").collect(),
    split.next().unwrap().split('\n').collect(),
  )
}

fn arrange(design: &str, patterns: &[&str]) -> usize {
  let mut stack: Vec<usize> = vec![0];
  let mut visited: HashSet<usize> = HashSet::new();
  let mut next_vertices: HashMap<usize, Vec<usize>> = HashMap::new();
  while let Some(pos) = stack.pop() {
    for &pat in patterns {
      let end = pos + pat.len();
      if end > design.len() {
        continue;
      }
      if &design[pos..end] == pat {
        if visited.insert(end) {
          stack.push(end);
        }
        next_vertices.entry(pos).or_default().push(end);
      }
    }
  }

  let mut edges_cnt_to: Vec<usize> = vec![0; design.len() + 1];
  edges_cnt_to[0] = 1;
  for pos in 0..design.len() {
    if let Some(vertices) = next_vertices.get(&pos) {
      for &v in vertices {
        edges_cnt_to[v] += edges_cnt_to[pos];
      }
    }
  }

  edges_cnt_to[design.len()]
}

fn solve(input: &str) -> usize {
  let (patterns, designs) = parse(input);
  designs
    .iter()
    .map(|&design| arrange(design, &patterns))
    .sum()
}

pub fn main() -> usize {
  let input = fs::read_to_string("input/in").unwrap();
  solve(input.trim())
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_arrange() {
    assert_eq!(
      arrange(
        "rrbgbr",
        "r, wr, b, g, bwu, rb, gb, br"
          .split(", ")
          .collect::<Vec<&str>>()
          .as_slice()
      ),
      6
    );
  }

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
    assert_eq!(solve(input), 16);
  }
}
