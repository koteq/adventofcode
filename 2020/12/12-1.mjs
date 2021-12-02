import { readFileSync } from "fs";

const instructions = String(readFileSync("./12-1.in"))
  .split("\n")
  .map((line) => [line[0], Number(line.slice(1))]);

let x = 0;
let y = 0;
let rot = 0;
const handlers = {
  N: (dist) => (y += dist),
  S: (dist) => (y -= dist),
  E: (dist) => (x += dist),
  W: (dist) => (x -= dist),
  R: (angle) => (rot += angle),
  L: (angle) => (rot -= angle),
  F: (dist) => {
    x += Math.round(Math.cos((Math.PI / 180) * -rot) * dist);
    y += Math.round(Math.sin((Math.PI / 180) * -rot) * dist);
  },
};

instructions.forEach(([op, val]) => handlers[op](val));
const manhattanDist = Math.abs(x) + Math.abs(y);
console.log(manhattanDist);
