const { readFileSync } = require("fs");

const input = String(readFileSync("./3-1.in")).split("\n");
const h = input.length;
const w = input[0].length;
const pos = [0, 0]; // x, y
const move = [3, 1]; // x, y
let treesCount = 0;

for (let i = 1; i < h; i++) {
  pos[0] = (pos[0] + move[0]) % w;
  pos[1] += move[1];
  const hasTree = input[pos[1]][pos[0]] === "#";
  if (hasTree) {
    treesCount += 1;
  }
}
console.log(treesCount);
