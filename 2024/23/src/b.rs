use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter};
use std::fs;
use std::str;

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Computer([u8; 2]);

impl Computer {
  const fn new(id: &str) -> Self {
    let bytes = id.as_bytes();
    Self([bytes[0], bytes[1]])
  }
}

impl Debug for Computer {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", str::from_utf8(&self.0).unwrap())
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

fn solve(input: &str) -> Vec<Computer> {
  let (connections, peers_map) = parse(input);

  let mut largest_net: Vec<Computer> = Vec::new();
  for start_comp in peers_map.keys() {
    let mut stack: Vec<Vec<Computer>> = vec![vec![*start_comp]];
    while let Some(net) = stack.pop() {
      let last_comp = net.last().unwrap();
      for next_comp in peers_map.get(last_comp).unwrap() {
        if next_comp > last_comp
          && net
            .iter()
            .all(|c| connections.contains(&Connection::new(*c, *next_comp)))
        {
          let mut next_net = net.clone(); // we have lots of RAM, right?
          next_net.push(*next_comp);
          if next_net.len() > largest_net.len() {
            largest_net.clone_from(&next_net);
          }
          stack.push(next_net);
        }
      }
    }
  }
  largest_net
}

pub fn main() -> Vec<Computer> {
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
    assert_eq!(
      solve(input),
      [
        Computer::new("co"),
        Computer::new("de"),
        Computer::new("ka"),
        Computer::new("ta")
      ]
    );
  }
}
