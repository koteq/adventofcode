import { readFileSync } from "fs";

const input = readFileSync("in").toString().trim().split(",").map(Number);

const moveCost = new Map([[0, 0]]);
const maxPos = Math.max(...input);
for (let i = 1; i <= maxPos; i++) {
  moveCost.set(i, moveCost.get(i - 1) + i);
}

function calcCost(positions, alignedPos) {
  return positions.reduce((acc, pos) => {
    const move = Math.abs(pos - alignedPos);
    return acc + moveCost.get(move);
  }, 0);
}

let leastFuel = Infinity;
for (let i = 0; i <= maxPos; i++) {
  leastFuel = Math.min(leastFuel, calcCost(input, i));
}

console.log(leastFuel);
