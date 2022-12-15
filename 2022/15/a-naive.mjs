import fs from "fs";

const targetRow = 10;
const sensors = fs
  .readFileSync("s1")
  .toString()
  .trim()
  .split("\n")
  .map((line) => Array.from(line.matchAll(/\d+/g)).map((m) => Number(m[0])));

const set = new Set();

for (const [sx, sy, bx, by] of sensors) {
  const distToBeacon = Math.abs(sx - bx) + Math.abs(sy - by);
  const distToRow = Math.abs(sy - targetRow);
  const offset = distToBeacon - distToRow;
  if (offset >= 0) {
    console.log(
      `\nDraw Sensor at x=${sx},\ty=${sy},\tdistToBeacon=${distToBeacon},\tdistToRow=${distToRow},\toffset=${offset}\n`
    );
    set.add(sx);
    for (let i = offset; i > 0; i--) {
      set.add(sx + i);
      set.add(sx - i);
    }
  } else {
    console.log(
      `Skip Sensor at x=${sx},\ty=${sy},\tdistToBeacon=${distToBeacon},\tdistToRow=${distToRow},\toffset=${offset}`
    );
  }
}

for (const [sx, sy, bx, by] of sensors) {
  if (sy === targetRow) {
    set.delete(sx);
  }
  if (by === targetRow) {
    set.delete(bx);
  }
}

console.log(set.size);

// 5037929 too low
// 5037930 too low
