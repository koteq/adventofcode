import { readFileSync } from "fs";

const input = String(readFileSync("./13-1.in")).split("\n");
const earliest = Number(input[0]);
const ids = input[1].split(",").map(Number).filter(Number.isFinite);

const schedule = ids
  .map((id) => [id, Math.ceil(earliest / id) * id])
  .sort(([, a], [, b]) => a - b);
const [nextId, nextTime] = schedule[0];

console.log(nextId * (nextTime - earliest));
