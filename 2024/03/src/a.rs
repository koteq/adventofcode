use regex::Regex;
use std::fs;

pub fn main() -> i32 {
    let input = fs::read_to_string("input/in").unwrap();
    let re = Regex::new(r"mul\((\d{1,3}),(\d{1,3})\)").unwrap();
    let mut result = 0;
    for (_, [a, b]) in re.captures_iter(&*input).map(|caps| caps.extract()) {
        result += a.parse::<i32>().unwrap() * b.parse::<i32>().unwrap();
    }
    result
}