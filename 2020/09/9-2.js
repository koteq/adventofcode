const { readFileSync } = require("fs");

const key = 167829540;
const input = String(readFileSync("./9-1.in")).split("\n").map(Number);
let buffer = 0;
let lo = 0;
let hi = 0;
while (true) {
  if (buffer === key) {
    const range = input.slice(lo, hi);
    const min = Math.min(...range);
    const max = Math.max(...range);
    console.log(min + max);
    process.exit(0);
  }
  if (buffer < key) {
    hi += 1;
  } else {
    lo += 1;
  }
  buffer = input.slice(lo, hi).reduce((acc, n) => acc + n, 0);
}
