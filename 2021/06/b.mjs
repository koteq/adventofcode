import { readFileSync } from "fs";

const state = readFileSync("in").toString().trim().split(",").map(Number);

function solve(initialDelay, days, initialPopulation = 1) {
  if (initialDelay === 5 && days === 15) {
    console.log();
  }
  let population = initialPopulation;
  const breedInterval = 7;
  const breedDelay = 2;
  population += Math.max(Math.ceil((days - initialDelay) / breedInterval), 0);
  for (
    let range = days - initialDelay - breedDelay - breedInterval;
    range > 0;
    range -= breedInterval
  ) {
    population += Math.ceil(range / breedInterval);
    for (let range2 = range; range2 > 0; range2 -= breedInterval) {
      population += solve(breedDelay + breedInterval, range2, 0);
    }
  }
  return population;
}

function solveBrute(number, days) {
  const state = [number];
  for (let day = 0; day < days; day++) {
    for (let i = 0; i < state.length; i++) {
      state[i] -= 1;
      if (state[i] < 0) {
        state[i] = 6;
        state.push(9);
      }
    }
  }

  return state.length;
}

if (0) {
  for (let s = 1; s <= 5; s++) {
    for (let d = 1; d < 80; d++) {
      const b = solveBrute(s, d);
      const f = solve(s, d);
      console.assert(b === f, `${b} !== ${f}, for s ${s} d ${d}`);
    }
  }
}

// Takes several minutes on MBP to calculate
const daysToSimulate = 256;
const solutions = [
  NaN,
  6206821033, //solve(1, 256),
  5617089148, //solve(2, 256),
  5217223242, //solve(3, 256),
  4726100874, //solve(4, 256),
  4368232009, //solve(5, 256),
];

const result = state.reduce((acc, fish) => acc + solutions[fish], 0);
console.log(result);
