import fs from "fs";

const heatMap = fs
  .readFileSync("in")
  .toString()
  .trim()
  .split("\n")
  .map((line) => line.split("").map(Number));

function isLowPoint(x, y) {
  const middle = heatMap[y][x];
  const adjacent = [
    heatMap[y - 1]?.[x],
    heatMap[y + 1]?.[x],
    heatMap[y][x - 1],
    heatMap[y][x + 1],
  ];
  return adjacent.every((value) => value == null || value > middle);
}

let riskLevel = 0;
for (let y = 0, maxY = heatMap.length; y < maxY; y++) {
  for (let x = 0, maxX = heatMap[0].length; x < maxX; x++) {
    if (isLowPoint(x, y)) {
      riskLevel += heatMap[y][x] + 1;
    }
  }
}
console.log(riskLevel);
