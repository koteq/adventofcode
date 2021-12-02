import { readFileSync } from "fs";

const input = readFileSync("in").toString().split("\n").map(Number);

const window3 = [];
for (let i = 2; i < input.length; i++) {
  window3.push(input[i] + input[i - 1] + input[i - 2]);
}

let increases = 0;
window3.reduce((previous, current) => {
  if (previous != null && previous < current) {
    increases += 1;
  }
  return current;
}, null);
console.log(increases);
