import fs from "fs";

const cavesMap = new Map();
const addPath = ([a, b]) => {
  cavesMap.set(a, [...(cavesMap.get(a) ?? []), b]);
  cavesMap.set(b, [...(cavesMap.get(b) ?? []), a]);
};
fs.readFileSync("in")
  .toString()
  .trim()
  .split("\n")
  .forEach((line) => addPath(line.split("-")));

const isBig = (cave) => /^[A-Z]+$/.test(cave);

let pathsCount = 0;
const allowedSmallCaveRevisits = 1;
const stack = [[0, "start"]];
while (stack.length) {
  const [smallCaveRevisitCount, ...path] = stack.pop();
  const lastCave = path[path.length - 1];
  if (lastCave === "end") {
    pathsCount += 1;
  } else {
    for (const adjacentCave of cavesMap.get(lastCave)) {
      const isVisited = path.includes(adjacentCave);
      if (isBig(adjacentCave) || !isVisited) {
        stack.push([smallCaveRevisitCount, ...path, adjacentCave]);
      } else if (
        !["start", "end"].includes(adjacentCave) &&
        smallCaveRevisitCount < allowedSmallCaveRevisits
      ) {
        stack.push([smallCaveRevisitCount + 1, ...path, adjacentCave]);
      }
    }
  }
}

console.log(pathsCount);
