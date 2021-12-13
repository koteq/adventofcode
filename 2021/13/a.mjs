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

// do the first fold
const instruction = instructions[0];
{
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

console.log(paper.size);
