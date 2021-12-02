const { readFileSync } = require("fs");

const input = String(readFileSync("./10-1.in"))
  .split("\n")
  .map(Number)
  .sort((a, b) => a - b);

const memory = [1, 1, 2, 4];
function calculatePossibleConnections(distance) {
  for (let i = memory.length; i <= distance; i++) {
    memory[i] = memory[i - 1] + memory[i - 2] + memory[i - 3];
  }
  return memory[distance];
}

let chainLength = 0;
let previousJoltage = 0;
let totalPossibleConnections = 1;
input.push(+Infinity);
input.forEach((joltage) => {
  const joltageDiff = joltage - previousJoltage;
  previousJoltage = joltage;
  if (joltageDiff === 1) {
    chainLength += 1;
  } else {
    totalPossibleConnections *= calculatePossibleConnections(chainLength);
    chainLength = 0;
  }
});
console.log(totalPossibleConnections);
