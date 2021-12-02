import { readFileSync } from "fs";

const instructions = String(readFileSync("./12-1.in"))
  .split("\n")
  .map((line) => [line[0], Number(line.slice(1))]);

function translate(x, y, angle) {
  const cos = Math.round(Math.cos((Math.PI / 180) * -angle));
  const sin = Math.round(Math.sin((Math.PI / 180) * -angle));
  return [x * cos + y * -sin, x * sin + y * cos];
}

let shipX = 0;
let shipY = 0;
let waypointX = 10;
let waypointY = 1;
const handlers = {
  N: (dist) => (waypointY += dist),
  S: (dist) => (waypointY -= dist),
  E: (dist) => (waypointX += dist),
  W: (dist) => (waypointX -= dist),
  R: (angle) =>
    ([waypointX, waypointY] = translate(waypointX, waypointY, angle)),
  L: (angle) =>
    ([waypointX, waypointY] = translate(waypointX, waypointY, -angle)),
  F: (times) => {
    shipX += waypointX * times;
    shipY += waypointY * times;
  },
};

instructions.forEach(([op, val]) => handlers[op](val));
const manhattanDist = Math.abs(shipX) + Math.abs(shipY);
console.log(manhattanDist);
// 145117
