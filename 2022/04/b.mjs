import fs from "fs";

function tap(arg) {
  console.log(arg);
  return arg;
}

function isOverlap([a, b]) {
  return !(
    (a[0] < b[0] && a[1] < b[0]) ||
    (a[0] > b[1] && a[1] > b[1])
  );
}

const result = fs
  .readFileSync("in")
  .toString()
  .trim()
  .split("\n")
  .map((l) => l.split(",").map((p) => p.split("-").map(Number)))
  // .map(tap)
  .filter(isOverlap).length;
console.log(result);
