use std::cmp::min;
use std::fs;

fn to_digit(char: &u8) -> u8 {
  char - b'0'
}

fn to_file_id(pos: &usize) -> usize {
  pos / 2
}

fn solve(raw_input: &[u8]) -> usize {
  let mut input: Vec<u8> = raw_input.iter().map(to_digit).collect();
  let mut sum: usize = 0;
  let mut block_pos = 0;
  let mut input_pos = 0;
  let mut is_input_pos_at_file = true;
  let mut input_pos_of_last_file = input.len() - 1;
  while input_pos <= input_pos_of_last_file {
    if is_input_pos_at_file {
      let file_id = to_file_id(&input_pos);
      let file_len = input[input_pos];
      sum += (block_pos..(block_pos + file_len as usize))
        .map(|pos| pos * file_id)
        .sum::<usize>();
      block_pos += file_len as usize;
    } else {
      let mut gap_len = input[input_pos];
      while gap_len > 0 && input_pos_of_last_file > input_pos {
        let last_file_id = to_file_id(&input_pos_of_last_file);
        let last_file_len = input[input_pos_of_last_file];
        sum += (block_pos..(block_pos + min(gap_len, last_file_len) as usize))
          .map(|pos| pos * last_file_id)
          .sum::<usize>();
        block_pos += min(gap_len, last_file_len) as usize;
        input[input_pos_of_last_file] = input[input_pos_of_last_file].saturating_sub(gap_len);
        if input[input_pos_of_last_file] == 0 {
          input_pos_of_last_file -= 2;
        }
        gap_len = gap_len.saturating_sub(last_file_len);
      }
    }
    input_pos += 1;
    is_input_pos_at_file = !is_input_pos_at_file;
  }
  sum
}

pub fn main() -> usize {
  let input = fs::read_to_string("input/in").unwrap();
  solve(input.trim().as_bytes())
}

#[cfg(test)]
mod tests {
  use super::*;
  
  #[test]
  fn test_solve_example_1() {
    assert_eq!(solve(b"12345"), 60);
  }

  #[test]
  fn test_solve_example_2() {
    assert_eq!(solve(b"2333133121414131402"), 1928);
  }
}
