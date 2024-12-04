use std::fs;

struct XmasPuzzle {
  input: Vec<String>,
}

impl XmasPuzzle {
  fn new(input: String) -> Self {
    Self {
      input: input.trim().split("\n").map(String::from).collect(),
    }
  }

  fn char_at(&self, x: i32, y: i32) -> Option<char> {
    let Ok(u_x) = usize::try_from(x) else { return None };
    let Ok(u_y) = usize::try_from(y) else { return None };
    self.input.get(u_y).and_then(|s| s.chars().nth(u_x))
  }

  pub fn count_xmas(&self) -> i32 {
    let mut result = 0;
    for c_y in 0..self.input.len() {
      for c_x in 0..self.input[0].len() {
        let directions: [(i32, i32); 8] = [
          (1, 0),
          (1, 1),
          (0, 1),
          (-1, 1),
          (-1, 0),
          (-1, -1),
          (0, -1),
          (1, -1),
        ];
        for (o_x, o_y) in directions.iter() {
          let mut word = String::new();
          for i in 0..4 {
            let x = c_x as i32 + *o_x * i as i32;
            let y = c_y as i32 + *o_y * i as i32;
            match self.char_at(x, y) {
              Some(char) => word.push(char),
              None => break,
            }
          }
          if word == "XMAS" {
            result += 1;
          }
        }
      }
    }
    result
  }
}

pub fn main() -> i32 {
  let input = fs::read_to_string("input/in").unwrap();
  let puzzle = XmasPuzzle::new(input);
  puzzle.count_xmas()
}
