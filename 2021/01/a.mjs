import { readFileSync } from "fs";

let increases = 0;
readFileSync("in")
  .toString()
  .split("\n")
  .map(Number)
  .reduce((previous, current) => {
    if (previous != null && previous < current) {
      increases += 1;
    }
    return current;
  }, null);
console.log(increases);
