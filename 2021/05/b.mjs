import { assert } from "console";
import { readFileSync } from "fs";

const input = readFileSync("in")
  .toString()
  .trim()
  .split("\n")
  .map((line) =>
    line.split(" -> ").map((coords) => coords.split(",").map(Number))
  );

/** @type {Map<string, number>} */
const map = new Map();
input.forEach(([[x1, y1], [x2, y2]]) => {
  if (x1 === x2) {
    range(y1, y2).forEach((y) => {
      const coords = [x1, y].join();
      map.set(coords, (map.get(coords) ?? 0) + 1);
    });
  } else if (y1 === y2) {
    range(x1, x2).forEach((x) => {
      const coords = [x, y1].join();
      map.set(coords, (map.get(coords) ?? 0) + 1);
    });
  } else {
    // ↘
    if (x1 < x2 && y1 < y2) {
      for (let i = 0; i <= x2 - x1; i++) {
        const coords = [x1 + i, y1 + i].join();
        map.set(coords, (map.get(coords) ?? 0) + 1);
        if (i === 0) assert(coords === [x1, y1].join());
        if (i === x2 - x1) assert(coords === [x2, y2].join());
      }
    }
    // ↙
    if (x1 > x2 && y1 < y2) {
      for (let i = 0; i <= x1 - x2; i++) {
        const coords = [x1 - i, y1 + i].join();
        map.set(coords, (map.get(coords) ?? 0) + 1);
        if (i === 0) assert(coords === [x1, y1].join());
        if (i === x1 - x2) assert(coords === [x2, y2].join());
      }
    }
    // ↖
    if (x1 > x2 && y1 > y2) {
      for (let i = 0; i <= x1 - x2; i++) {
        const coords = [x1 - i, y1 - i].join();
        map.set(coords, (map.get(coords) ?? 0) + 1);
        if (i === 0) assert(coords === [x1, y1].join());
        if (i === x1 - x2) assert(coords === [x2, y2].join());
      }
    }
    // ↗
    if (x1 < x2 && y1 > y2) {
      for (let i = 0; i <= x2 - x1; i++) {
        const coords = [x1 + i, y1 - i].join();
        map.set(coords, (map.get(coords) ?? 0) + 1);
        if (i === 0) assert(coords === [x1, y1].join());
        if (i === x2 - x1) assert(coords === [x2, y2].join());
      }
    }
  }
});

let overlaps = 0;
map.forEach((val) => {
  if (val > 1) {
    overlaps += 1;
  }
});

// 5698 is too low
// 13624 is too low
// 15446 is too low
console.log(overlaps);

function range(...args) {
  const [a, b] = args.sort((a, b) => a - b);
  const result = [];
  for (let i = a; i <= b; i++) {
    result.push(i);
  }
  return result;
}
