const { readFileSync } = require("fs");

const input = String(readFileSync("./3-1.in")).split("\n");
const h = input.length;
const w = input[0].length;

function ride(move) {
  let treesCount = 0;
  const pos = [0, 0]; // x, y
  while (pos[1] < h - 1) {
    pos[0] = (pos[0] + move[0]) % w;
    pos[1] += move[1];
    const hasTree = input[pos[1]][pos[0]] === "#";
    if (hasTree) {
      treesCount += 1;
    }
  }
  return treesCount;
}

console.log(
  ride([1, 1]) * ride([3, 1]) * ride([5, 1]) * ride([7, 1]) * ride([1, 2])
);
