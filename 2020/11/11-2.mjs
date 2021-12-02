import { readFileSync } from "fs";

const xy = (x, y) => [x, y].join(",");

function readMap(str) {
  const input = str.split("\n");
  const h = input.length;
  const w = input[0].length;

  let map = new Map();
  input.map((line, y) =>
    Array.from(line).forEach(
      (chr, x) => chr === "L" && map.set(xy(x, y), false)
    )
  );

  return [map, h, w];
}

let [map, h, w] = readMap(String(readFileSync("./11-1.in")));
function calcAdjacentOccupied(map, xo, yo) {
  let result = 0;
  for (let y = -1; y <= 1; y++) {
    for (let x = -1; x <= 1; x++) {
      if (!(x === 0 && y === 0)) {
        for (let d = 1; d <= Math.max(w, h); d++) {
          if (map.has(xy(xo + x * d, yo + y * d))) {
            result += map.get(xy(xo + x * d, yo + y * d)) ? 1 : 0;
            break;
          }
        }
      }
    }
  }
  return result;
}

function printMap(map) {
  console.log("\n");
  for (let y = 0; y < h; y++) {
    const line = [];
    for (let x = 0; x < w; x++) {
      if (map.has(xy(x, y))) {
        line.push(map.get(xy(x, y)) ? "#" : "L");
      } else {
        line.push(".");
      }
    }
    console.log(line.join(""));
  }
}

while (true) {
  const old = map;
  map = new Map();

  let changed = false;
  for (let y = 0; y < h; y++) {
    for (let x = 0; x < w; x++) {
      if (old.has(xy(x, y))) {
        let value = old.get(xy(x, y));
        const adj = calcAdjacentOccupied(old, x, y);
        if (value === false && adj === 0) {
          value = true;
          changed = true;
        } else if (value === true && adj >= 5) {
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
