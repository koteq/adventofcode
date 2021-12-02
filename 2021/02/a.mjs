import { readFileSync } from "fs";

const lineParser = (line) => {
  const [cmd, val] = line.split(" ");
  return [cmd, Number(val)];
};

let horizontalPosition = 0;
let depth = 0;

const actions = {
  forward: (val) => (horizontalPosition += val),
  down: (val) => (depth += val),
  up: (val) => (depth -= val),
};

readFileSync("in")
  .toString()
  .split("\n")
  .map(lineParser)
  .forEach(([cmd, val]) => {
    actions[cmd](val);
  });

console.log({ horizontalPosition, depth, result: horizontalPosition * depth });
