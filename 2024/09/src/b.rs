use std::fs;

fn to_digit(char: &u8) -> u8 {
  char - b'0'
}

fn to_file_id(pos: &usize) -> usize {
  pos / 2
}

enum Allocation {
  File { file_id: usize, file_len: u8 },
  Space { space_len: u8 },
}

fn solve(raw_input: &[u8]) -> usize {
  let mut disk: Vec<Allocation> = raw_input
    .iter()
    .enumerate()
    .map(|(pos, char)| {
      if pos % 2 == 0 {
        Allocation::File {
          file_id: to_file_id(&pos),
          file_len: to_digit(char),
        }
      } else {
        Allocation::Space { space_len: to_digit(char) }
      }
    })
    .collect();
  let mut offset = 0;
  while offset < disk.len() {
    let from_pos = disk.len() - offset - 1;
    if let Allocation::File { file_id, file_len } = disk[from_pos] {
      for to_pos in 0..from_pos {
        if let Allocation::Space { space_len } = disk[to_pos] {
          if space_len == file_len {
            disk[from_pos] = Allocation::Space { space_len: file_len };
            disk[to_pos] = Allocation::File { file_id, file_len };
            break;
          }
          if space_len > file_len {
            disk[from_pos] = Allocation::Space { space_len: file_len };
            disk[to_pos] = Allocation::Space { space_len: space_len - file_len };
            disk.insert(to_pos, Allocation::File { file_id, file_len });
            break;
          }
        }
      }
    }
    offset += 1;
  }
  let mut sum: usize = 0;
  let mut block_pos: usize = 0;
  for allocation in disk {
    match allocation {
      Allocation::File { file_id, file_len } => {
        sum += (block_pos..(block_pos + file_len as usize))
          .map(|pos| pos * file_id)
          .sum::<usize>();
        block_pos += file_len as usize;
      },
      Allocation::Space { space_len } => {
        block_pos += space_len as usize;
      }
    }
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
  fn test_solve_example_2() {
    assert_eq!(solve(b"2333133121414131402"), 2858);
  }
}
