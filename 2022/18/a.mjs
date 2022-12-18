import fs from "fs";

const data = fs.readFileSync("in").toString().trim().split("\n");
const lines = new Set(data);

let result = 0;
for (const line of lines) {
  const [x, y, z] = line.split(",").map(Number);
  const adjacent = [
    [x + 1, y + 0, z + 0],
    [x - 1, y + 0, z + 0],
    [x + 0, y + 1, z + 0],
    [x + 0, y - 1, z + 0],
    [x + 0, y + 0, z + 1],
    [x + 0, y + 0, z - 1],
  ]
    .map((coords) => (lines.has(coords.join(",")) ? 1 : 0))
    .reduce((acc, n) => acc + n, 0);

  result += 6 - adjacent;
}
console.log(result);
