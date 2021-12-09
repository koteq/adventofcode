import fs from "fs";

let maxX = 0;
let maxY = 0;
const basinMap = new Map();
const maxBasinHeight = 8;
fs.readFileSync("in")
  .toString()
  .trim()
  .split("\n")
  .forEach((line, y) => {
    maxY = Math.max(maxY, y);
    line
      .split("")
      .map(Number)
      .forEach((value, x) => {
        maxX = Math.max(maxX, x);
        if (value <= maxBasinHeight) {
          basinMap.set([x, y].join(), value);
        }
      });
  });

const basinSizes = [];
for (let y = 0; y < maxY; y++) {
  for (let x = 0; x < maxX; x++) {
    const basinSize = removeBasin(x, y);
    if (basinSize) {
      basinSizes.push(basinSize);
    }
  }
}

function removeBasin(x, y) {
  const point = [x, y].join();
  if (basinMap.has(point)) {
    basinMap.delete(point);
    const adjacent = [
      [x, y - 1],
      [x, y + 1],
      [x - 1, y],
      [x + 1, y],
    ];
    return (
      1 +
      adjacent
        .map(([x, y]) => removeBasin(x, y))
        .reduce((acc, val) => acc + val, 0)
    );
  }
  return 0;
}
const answer = basinSizes
  .sort((a, b) => b - a)
  .slice(0, 3)
  .reduce((acc, val) => acc * val, 1);
console.log(answer);
