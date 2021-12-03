import { readFileSync } from "fs";

let linesCount = 0;
let counters = [];
readFileSync("in")
  .toString()
  .split("\n")
  .forEach((line) => {
    Array.from(line).forEach((char, pos) => {
      counters[pos] ??= 0;
      counters[pos] += Number(char);
    });
    linesCount += 1;
  });

let gammaRate = 0;
const totalBits = counters.length;
// let epsilon = [];
counters.forEach((count, pos) => {
  if (count > linesCount / 2) {
    gammaRate += 1 << (totalBits - pos - 1);
  }
});
const epsilonRate = (1 << totalBits) - gammaRate - 1;

console.log({ gammaRate, epsilonRate, result: gammaRate * epsilonRate });
