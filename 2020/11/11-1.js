const { readFileSync } = require("fs");

const xy = (x, y) => [x, y].join(",");

const input = String(readFileSync("./11-1.in")).split("\n");
const h = input.length;
const w = input[0].length;

let map = new Map();
input.map((line, y) =>
  Array.from(line).forEach((chr, x) => chr === "L" && map.set(xy(x, y), true))
);

function calcAdjacentOccupied(map, xo, yo) {
  let result = 0;
  for (let y = -1; y <= 1; y++) {
    for (let x = -1; x <= 1; x++) {
      if (!(x === 0 && y === 0)) {
        result += map.get(xy(xo + x, yo + y)) ? 1 : 0;
      }
    }
  }
  return result;
}

while (true) {
  const old = map;
  map = new Map();

  let changed = false;
  for (let y = 0; y < h; y++) {
    for (let x = 0; x < w; x++) {
      if (old.has(xy(x, y))) {
        let value = old.get(xy(x, y));
        if (value === false && calcAdjacentOccupied(old, x, y) === 0) {
          value = true;
          changed = true;
        } else if (value === true && calcAdjacentOccupied(old, x, y) >= 4) {
          value = false;
          changed = true;
        }
        map.set(xy(x, y), value);
      }
    }
  }

  if (!changed) {
    console.log(Array.from(map.values()).filter((e) => e).length);
    process.exit(0);
  }
}
