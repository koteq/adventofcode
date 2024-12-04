use std::fs;

struct XmasPuzzle {
  input: Vec<String>,
}

impl XmasPuzzle {
  // fn new(input: String) -> Self {}
  fn new(input: String) -> Self {
    Self {
      input: input.trim().split("\n").map(String::from).collect(),
    }
  }

  fn char_at(&self, x: i32, y: i32) -> Option<char> {
    let Ok(u_x) = usize::try_from(x) else {
      return None;
    };
    let Ok(u_y) = usize::try_from(y) else {
      return None;
    };
    self.input.get(u_y).and_then(|s| s.chars().nth(u_x))
  }

  pub fn count_xmas(&self) -> i32 {
    let mut result = 0;
    for c_y in 0..self.input.len() as i32 {
      for c_x in 0..self.input[0].len() as i32 {
        match self.char_at(c_x, c_y) {
          Some('A') => {
            let a = self.char_at(c_x - 1, c_y - 1).unwrap_or('.');
            let b = self.char_at(c_x + 1, c_y + 1).unwrap_or('.');
            let x = self.char_at(c_x + 1, c_y - 1).unwrap_or('.');
            let y = self.char_at(c_x - 1, c_y + 1).unwrap_or('.');
            if ((a == 'M' && b == 'S') || (a == 'S' && b == 'M'))
              && ((x == 'M' && y == 'S') || (x == 'S' && y == 'M'))
            {
              result += 1;
            }
          }
          _ => {}
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
