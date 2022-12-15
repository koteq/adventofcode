import fs from "fs";

const targetRow = 2000000;
const sensors = fs
  .readFileSync("in")
  .toString()
  .trim()
  .split("\n")
  .map((line) => Array.from(line.matchAll(/-?\d+/g)).map((m) => Number(m[0])));

const ranges = [];

for (const [sx, sy, bx, by] of sensors) {
  const distToBeacon = Math.abs(sx - bx) + Math.abs(sy - by);
  const distToRow = Math.abs(sy - targetRow);
  const offset = distToBeacon - distToRow;
  if (offset >= 0) {
    ranges.push([sx - offset, sx + offset]);
  }
}

ranges.sort(([a], [b]) => a - b);
// console.log(ranges);

let pos = 0;
while (pos + 1 < ranges.length) {
  const a = ranges[pos];
  const b = ranges[pos + 1];
  if (a[1] < b[0]) {
    pos += 1;
  } else {
    ranges.splice(pos + 1, 1);
    a[1] = Math.max(a[1], b[1]);
  }
}

// console.log(ranges);
console.log(ranges.reduce((acc, [a, b]) => acc + (b - a), 0));
