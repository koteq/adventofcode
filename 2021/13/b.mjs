import fs from "fs";

/** @type {Map<string, number>} */
const paper = new Map();
/** @type {[orientation: "x" | "y", offset: number][]} */
const instructions = [];
{
  const [paperSection, instructionsSection] = fs
    .readFileSync("in")
    .toString()
    .trim()
    .split("\n\n");
  paperSection.split("\n").forEach((coords) => paper.set(coords, 1));
  instructionsSection.split("\n").forEach((line) => {
    const [orientation, offset] = line.split("=");
    instructions.push([orientation, Number(offset)]);
  });
}

// fold paper
for (const instruction of instructions) {
  const [orientation, offset] = instruction;
  paper.forEach((value, coords) => {
    const [x, y] = coords.split(",").map(Number);
    const isHorizontal = orientation === "fold along x";
    if ((isHorizontal && x > offset) || (!isHorizontal && y > offset)) {
      paper.delete(coords);
      const newCoords = (
        isHorizontal ? [offset - (x - offset), y] : [x, offset - (y - offset)]
      ).join(",");
      paper.set(newCoords, (paper.get(newCoords) ?? 0) + value);
    }
  });
}

// print code
let maxX = 0;
let maxY = 0;
paper.forEach((value, coords) => {
  const [x, y] = coords.split(",").map(Number);
  maxX = Math.max(maxX, x);
  maxY = Math.max(maxY, y);
});
const dots = [];
for (let y = 0; y <= maxY; y++) {
  dots[y] = [];
  for (let x = 0; x <= maxX; x++) {
    dots[y][x] = paper.has([x, y].join(",")) ? "#" : ".";
  }
}
console.log(dots.map((row) => row.join("")).join("\n"));
