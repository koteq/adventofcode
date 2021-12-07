import { readFileSync } from "fs";

const input = readFileSync("in").toString().trim().split(",").map(Number);

const medianPos = input.sort((a, b) => a - b)[Math.round(input.length / 2)];
const fuelCost = input.reduce((acc, pos) => acc + Math.abs(pos - medianPos), 0);
console.log({ medianPos, fuelCost });
