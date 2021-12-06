import { readFileSync } from "fs";

const state = readFileSync("in").toString().trim().split(",").map(Number);

const daysToSimulate = 80;
for (let day = 0; day < daysToSimulate; day++) {
  for (let i = 0; i < state.length; i++) {
    state[i] -= 1;
    if (state[i] < 0) {
      state[i] = 6;
      state.push(9);
    }
  }
}

console.log(state.length);
