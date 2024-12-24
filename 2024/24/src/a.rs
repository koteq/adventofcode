use std::collections::HashMap;
use std::fs;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Wire {
  id: [u8; 3],
}

impl Wire {
  fn new(input: &str) -> Self {
    Self {
      id: input.as_bytes().try_into().unwrap(),
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum GateKind {
  AND,
  OR,
  XOR,
}

impl GateKind {
  fn new(input: &str) -> Self {
    match input {
      "AND" => Self::AND,
      "OR" => Self::OR,
      "XOR" => Self::XOR,
      _ => unreachable!(),
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Gate {
  kind: GateKind,
  input: (Wire, Wire),
  output: Wire,
}

impl Gate {
  fn new(input: &str) -> Self {
    let split: Vec<&str> = input.split(' ').collect();
    Self {
      kind: GateKind::new(split[1]),
      input: (Wire::new(split[0]), Wire::new(split[2])),
      output: Wire::new(split[4]),
    }
  }
}

fn parse(input: &str) -> (HashMap<Wire, bool>, Vec<Gate>) {
  let mut wires: HashMap<Wire, bool> = HashMap::new();
  let mut gates: Vec<Gate> = Vec::new();
  let (wires_input, gates_input) = input.split_once("\n\n").unwrap();
  for line in wires_input.lines() {
    let (wire_id, wire_state) = line.split_once(": ").unwrap();
    wires.insert(Wire::new(wire_id), wire_state == "1");
  }
  for line in gates_input.lines() {
    gates.push(Gate::new(line));
  }
  (wires, gates)
}

fn solve(input: &str) -> u64 {
  let (mut wires, gates) = parse(input);

  // update gates until it settles
  loop {
    let mut changed = false;
    for gate in &gates {
      if let (Some(a), Some(b)) = (wires.get(&gate.input.0), wires.get(&gate.input.1)) {
        let value = match gate.kind {
          GateKind::AND => a & b,
          GateKind::OR => a | b,
          GateKind::XOR => a ^ b,
        };
        changed = changed || wires.insert(gate.output, value).is_none_or(|v| v != value);
      }
    }
    if !changed {
      break;
    }
  }

  // get the output
  let mut z_wires: Vec<Wire> = wires
    .keys()
    .filter(|w| w.id.starts_with(b"z"))
    .copied()
    .collect();
  z_wires.sort();
  let mut result: u64 = 0;
  for (pos, wire) in z_wires.iter().enumerate() {
    let value: u64 = wires.get(wire).unwrap().to_owned().into();
    result ^= value << pos;
  }
  result
}

pub fn main() -> u64 {
  let input = fs::read_to_string("input/in").unwrap();
  solve(input.trim())
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_solve_example_1() {
    let input = "\
x00: 1
x01: 1
x02: 1
y00: 0
y01: 1
y02: 0

x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02";
    assert_eq!(solve(input), 4);
  }

  #[test]
  fn test_solve_example_2() {
    let input = "\
x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj";
    assert_eq!(solve(input), 2024);
  }
}
