const { readFileSync } = require("fs");

const input = String(readFileSync("./10-1.in"))
  .split("\n")
  .map(Number)
  .sort((a, b) => a - b);
let cur = 0;
const diff = {
  1: 0,
  2: 0,
  3: 1,
};
input.forEach((n) => {
  const d = n - cur;
  diff[d] += 1;
  cur = n;
});
console.log(diff);
console.log(diff[1] * diff[3]);
