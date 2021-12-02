import { readFileSync } from "fs";

const lineParser = (line) => {
  const [cmd, val] = line.split(" ");
  return [cmd, Number(val)];
};

let horizontalPosition = 0;
let depth = 0;
let aim = 0;

const actions = {
  forward: (val) => {
    horizontalPosition += val;
    depth += aim * val;
  },
  down: (val) => (aim += val),
  up: (val) => (aim -= val),
};

readFileSync("in")
  .toString()
  .split("\n")
  .map(lineParser)
  .forEach(([cmd, val]) => {
    actions[cmd](val);
  });

console.log({
  horizontalPosition,
  depth,
  aim,
  result: horizontalPosition * depth,
});
