import fs from "fs";

const data = fs
  .readFileSync("in")
  .toString()
  .trim()
  .split("\n")
  .map((line) => line.split(" -> ").map((xy) => xy.split(",").map(Number)));

let sandCnt = 0;
let yMax = 0;
const state = new Map();
const STONE = 1;
const SAND = 2;

data.forEach((line) =>
  line.forEach(([xt, yt], idx, arr) => {
    if (idx > 0) {
      let [x, y] = arr[idx - 1];
      const xi = Math.sign(xt - x);
      const yi = Math.sign(yt - y);
      state.set(`${x},${y}`, STONE);
      if (y > yMax) yMax = y;
      do {
        x += xi;
        y += yi;
        state.set(`${x},${y}`, STONE);
        if (y > yMax) yMax = y;
      } while (x != xt || y != yt);
    }
  })
);
const yFloor = yMax + 2;

do {} while (addSand());
console.log(sandCnt);

function addSand() {
  const pos = findRestPos();
  state.set(`${pos[0]},${pos[1]}`, SAND);
  sandCnt += 1;
  const restAtOrigin = pos[0] === 500 && pos[1] === 0;
  return !restAtOrigin;
}

function findRestPos() {
  let x = 500;
  let y = 0;

  while (true) {
    if (!state.has(`${x},${y + 1}`) && y + 1 < yFloor) {
      y += 1;
    } else if (!state.has(`${x - 1},${y + 1}`) && y + 1 < yFloor) {
      y += 1;
      x -= 1;
    } else if (!state.has(`${x + 1},${y + 1}`) && y + 1 < yFloor) {
      y += 1;
      x += 1;
    } else {
      return [x, y];
    }
  }
}
