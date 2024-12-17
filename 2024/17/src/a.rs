use std::fs;

enum Op {
  ADV,
  BXL,
  BST,
  JNZ,
  BXC,
  OUT,
  BDV,
  CDV,
}

impl TryFrom<u8> for Op {
  type Error = ();

  fn try_from(value: u8) -> Result<Self, Self::Error> {
    match value {
      0 => Ok(Self::ADV),
      1 => Ok(Self::BXL),
      2 => Ok(Self::BST),
      3 => Ok(Self::JNZ),
      4 => Ok(Self::BXC),
      5 => Ok(Self::OUT),
      6 => Ok(Self::BDV),
      7 => Ok(Self::CDV),
      _ => Err(()),
    }
  }
}

struct Computer {
  a: i32,
  b: i32,
  c: i32,
  program_pointer: usize,
}

impl Computer {
  fn new(a: i32, b: i32, c: i32) -> Self {
    Self {
      a,
      b,
      c,
      program_pointer: 0,
    }
  }

  fn run(&mut self, program: &[u8]) -> Vec<u8> {
    let mut output: Vec<u8> = Vec::new();
    while self.program_pointer < program.len() {
      let operation: Op = program[self.program_pointer].try_into().unwrap();
      let operand = program[self.program_pointer + 1];
      match operation {
        Op::ADV => self.a /= 2_i32.pow(u32::try_from(self.combo(operand)).unwrap()),
        Op::BXL => self.b ^= i32::from(operand),
        Op::BST => self.b = self.combo(operand) % 8,
        Op::JNZ => {
          if self.a != 0 {
            self.program_pointer = operand as usize;
            continue;
          }
        }
        Op::BXC => self.b ^= self.c,
        Op::OUT => output.push(u8::try_from(self.combo(operand) % 8).unwrap()),
        Op::BDV => self.b = self.a / 2_i32.pow(u32::try_from(self.combo(operand)).unwrap()),
        Op::CDV => self.c = self.a / 2_i32.pow(u32::try_from(self.combo(operand)).unwrap()),
      }
      self.program_pointer += 2;
    }
    output
  }

  fn combo(&self, value: u8) -> i32 {
    match value {
      0..=3 => value.into(),
      4 => self.a,
      5 => self.b,
      6 => self.c,
      _ => unreachable!(),
    }
  }
}

fn parse_registers(input: &str) -> Computer {
  let mut split = input
    .split(|c| !char::is_ascii_digit(&c))
    .filter_map(|n| str::parse::<i32>(n).ok());
  Computer::new(
    split.next().unwrap(),
    split.next().unwrap(),
    split.next().unwrap(),
  )
}

fn parse_program(input: &str) -> Vec<u8> {
  input
    .bytes()
    .filter(u8::is_ascii_digit)
    .map(|c| c - b'0')
    .collect()
}

fn parse(input: &str) -> (Computer, Vec<u8>) {
  let mut split = input.split("\n\n");
  (
    parse_registers(split.next().unwrap()),
    parse_program(split.next().unwrap()),
  )
}

pub fn main() -> Vec<u8> {
  let input = fs::read_to_string("input/in").unwrap();
  let (mut computer, program) = parse(input.trim());
  computer.run(&program)
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_solve_example_1() {
    let input = "\
Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0";
    let (mut computer, program) = parse(input);
    assert_eq!(computer.run(&program), [4, 6, 3, 5, 6, 3, 5, 2, 1, 0]);
  }
}
