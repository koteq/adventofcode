import fs from "fs";

const data = fs
  .readFileSync("in")
  .toString()
  .trim()
  .split("\n")
  .map((l) => Array.from(l).map(Number));
const scores = data.map((l) => l.map(() => 0));
data.forEach((l, y) =>
  l.forEach((n, x) => {
    let score = 0;
    let tn = 0;
    let cn = 0;
    let cx = x;
    let cy = y;
    while (cy > 0 && tn < n) {
      cy -= 1;
      tn = data[cy][cx];
      // if (tn >= cn) {
        score += 1;
        cn = tn;
      // }
    }
    scores[y][x] = score;
    score = 0;
    tn = 0;
    cn = 0;
    cx = x;
    cy = y;
    while (cy < data.length - 1 && tn < n) {
      cy += 1;
      tn = data[cy][cx];
      // if (tn >= cn) {
        score += 1;
        cn = tn;
      // }
    }
    scores[y][x] *= score;
    score = 0;
    tn = 0;
    cn = 0;
    cx = x;
    cy = y;
    while (cx > 0 && tn < n) {
      cx -= 1;
      tn = data[cy][cx];
      // if (tn >= cn) {
        score += 1;
        cn = tn;
      // }
    }
    scores[y][x] *= score;
    score = 0;
    tn = 0;
    cn = 0;
    cx = x;
    cy = y;
    while (cx < data[0].length - 1 && tn < n) {
      cx += 1;
      tn = data[cy][cx];
      // if (tn >= cn) {
        score += 1;
        cn = tn;
      // }
    }
    scores[y][x] *= score;
  })
);

// 1152 too low
console.log(scores);
const result = scores
  .map((l) => l.reduce((acc, s) => Math.max(acc, s), 0))
  .reduce((acc, s) => Math.max(acc, s), 0);
console.log(result);
