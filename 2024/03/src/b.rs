use regex::Regex;
use std::fs;

pub fn main() -> i32 {
    let input = fs::read_to_string("input/in").unwrap();
    let re = Regex::new(r"(do)\(()\)|(don't)\(()\)|(mul)\((\d{1,3},\d{1,3})\)").unwrap();
    let mut enabled = true;
    let mut result = 0;
    for (_, [instruction, args]) in re.captures_iter(&*input).map(|caps| caps.extract()) {
        match instruction {
            "do" => enabled = true,
            "don't" => enabled = false,
            "mul" => {
                if enabled {
                    result += args
                        .split(",")
                        .map(|arg| arg.parse::<i32>().unwrap())
                        .reduce(|a, b| a * b)
                        .unwrap();
                }
            }
            _ => panic!("Unexpected instruction {}", instruction),
        }
    }
    result
}
