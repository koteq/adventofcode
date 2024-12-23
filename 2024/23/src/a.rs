use std::collections::{HashMap, HashSet};
use std::fs;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Computer([u8; 2]);

impl Computer {
  const fn new(id: &str) -> Self {
    let bytes = id.as_bytes();
    Self([bytes[0], bytes[1]])
  }

  const fn starts_with(self, c: u8) -> bool {
    self.0[0] == c
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Connection(Computer, Computer);

impl Connection {
  fn new(a: Computer, b: Computer) -> Self {
    // order the bidirectional connection to make the Eq and Hash traits work
    if a < b {
      Self(a, b)
    } else {
      Self(b, a)
    }
  }

  fn from_str(input: &str) -> Self {
    let (a, b) = input.split_once('-').unwrap();
    Self::new(Computer::new(a), Computer::new(b))
  }

  const fn peers(self) -> [Computer; 2] {
    [self.0, self.1]
  }
}

fn parse(input: &str) -> (HashSet<Connection>, HashMap<Computer, Vec<Computer>>) {
  let mut connections: HashSet<Connection> = HashSet::new();
  let mut peers: HashMap<Computer, Vec<Computer>> = HashMap::new();
  for line in input.lines() {
    let connection = Connection::from_str(line);
    connections.insert(connection);
    let [a, b] = connection.peers();
    peers.entry(a).or_default().push(b);
    peers.entry(b).or_default().push(a);
  }
  (connections, peers)
}

fn solve(input: &str) -> usize {
  let (connections, peers_map) = parse(input);

  let mut result = 0;
  for (first_comp, first_peers) in &peers_map {
    for second_comp in first_peers {
      if second_comp > first_comp {
        continue;
      }
      for third_comp in peers_map.get(second_comp).unwrap() {
        if third_comp > second_comp {
          continue;
        }
        if connections.contains(&Connection::new(*first_comp, *third_comp))
          && (first_comp.starts_with(b't')
            || second_comp.starts_with(b't')
            || third_comp.starts_with(b't'))
        {
          result += 1;
        }
      }
    }
  }
  result
}

pub fn main() -> usize {
  let input = fs::read_to_string("input/in").unwrap();
  solve(input.trim())
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_connection() {
    assert_eq!(Connection::from_str("kh-tc"), Connection::from_str("tc-kh"));
  }

  #[test]
  fn test_solve_example_1() {
    let input = "\
kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn";
    assert_eq!(solve(input), 7);
  }
}
