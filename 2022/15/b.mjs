import fs from "fs";

const sensors = fs
  .readFileSync("in")
  .toString()
  .trim()
  .split("\n")
  .map((line) => Array.from(line.matchAll(/-?\d+/g)).map((m) => Number(m[0])));

for (let y = 0; y < 4_000_000; y++) {
  const ranges = [];

  for (const [sx, sy, bx, by] of sensors) {
    const distToBeacon = Math.abs(sx - bx) + Math.abs(sy - by);
    const distToRow = Math.abs(sy - y);
    const offset = distToBeacon - distToRow;
    if (offset >= 0) {
      ranges.push([sx - offset, sx + offset]);
    }
  }

  ranges.sort(([a], [b]) => a - b);

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

  // might not work for every puzzle input
  if (ranges.length > 1) {
    const x = ranges[0][1] + 1;
    console.log(x * 4000000 + y);
    process.exit(0);
  }
}
