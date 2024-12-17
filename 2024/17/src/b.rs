use crate::a;

/*
BST,4: b = a % 8    ; b = {take 3 last bits from a}
BXL,7: b ^= 7       ; b = 7 - b
CDV,5: c = a / 2^b  ; c = {take all but $b last bits from a}
ADV,3: a /= 8       ; a = {remove 3 last bits from a}
BXL,7: b ^= 7       ; b = 7 - b
BXC,1: b ^= c       ; b = b ^ c
OUT,5: print(b % 8) ;
JNZ,0: until(a != 0)
*/

fn run(a: u64) -> Vec<u8> {
  let input = include_str!("../input/in");
  let (mut computer, program) = a::parse(input.trim());
  computer.a = a;
  computer.run(&program)
}

pub fn main() {
  const TARGET: [u8; 16] = [2, 4, 1, 7, 7, 5, 0, 3, 1, 7, 4, 1, 5, 5, 3, 0];
  let mut buff: Vec<u64> = Vec::from_iter([0]);
  for pos in 0..16 {
    let mut new_buff: Vec<u64> = Vec::new();
    for prev in buff {
      for i in 0..8 {
        let a = prev + i;
        if run(a)[0] == TARGET[15 - pos] {
          new_buff.push(a);
        }
      }
    }
    if pos == 15 {
      buff = new_buff;
      break;
    }
    buff = new_buff.iter().map(|v| v * 8).collect();
  }
  println!("Solutions {:?}", buff);
  println!("Optimal {:?}", buff[0]);
  println!("TRG {:?}", TARGET);
  println!("RUN {:?}", run(buff[0]));
}
