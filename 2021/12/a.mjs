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
const stack = [["start"]];
while (stack.length) {
  const path = stack.pop();
  const lastCave = path[path.length - 1];
  if (lastCave === "end") {
    pathsCount += 1;
  } else {
    for (const adjacentCave of cavesMap.get(lastCave)) {
      if (isBig(adjacentCave) || !path.includes(adjacentCave)) {
        stack.push([...path, adjacentCave]);
      }
    }
  }
}

console.log(pathsCount);
