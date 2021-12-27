import fs from "fs";

const map = new Map();
let rMove = new Set();
let dMove = new Set();
let maxX;
let maxY;
const w = ([x, y]) =>
  [Math.abs((x + maxX) % maxX), Math.abs((y + maxY) % maxY)].toString();
const m = () =>
  [...Array(maxY)].map((_, y) =>
    [...Array(maxX)].map((_, x) => map.get([x, y].toString()) ?? ".").join("")
  );
{
  const data = fs.readFileSync("in").toString().trim().split("\n");
  maxY = data.length;
  maxX = data[0].length;
  data.forEach((line, y) => {
    Array.from(line).forEach((char, x) => {
      if (char !== ".") {
        map.set([x, y].toString(), char);
      }
    });
  });
  map.forEach((char, coords) => {
    const [x, y] = coords.split(",").map(Number);
    if (char === ">") {
      if (!map.has(w([x + 1, y]))) {
        rMove.add([x, y].toString());
      }
    } else {
      if (!map.has(w([x, y + 1]))) {
        dMove.add([x, y].toString());
      }
    }
  });
}

let step = 0;
while (rMove.size || dMove.size) {
  // console.log("Step", step+1, "\n");
  // console.log(m().join("\n"), "\n");

  const rMoveCopy = new Set(rMove);
  rMove = new Set();
  for (const coords of rMoveCopy) {
    const [x, y] = coords.split(",").map(Number);
    map.delete(coords);
    map.set(w([x + 1, y]), ">");
    dMove.delete(w([x + 1, y - 1]));
    if (!map.has(w([x + 2, y]))) {
      rMove.add(w([x + 1, y]));
    }
    if (map.get(w([x, y - 1])) === "v") {
      dMove.add(w([x, y - 1]));
    } else if (map.get(w([x - 1, y])) === ">") {
      rMove.add(w([x - 1, y]));
    }
  }
  const dMoveCopy = new Set(dMove);
  dMove = new Set();
  for (const coords of dMoveCopy) {
    const [x, y] = coords.split(",").map(Number);
    map.delete(coords);
    map.set(w([x, y + 1]), "v");
    rMove.delete(w([x - 1, y + 1]));
    if (!map.has(w([x, y + 2]))) {
      dMove.add(w([x, y + 1]));
    }
    if (map.get(w([x - 1, y])) === ">") {
      rMove.add(w([x - 1, y]));
    } else if (map.get(w([x, y - 1])) === "v") {
      dMove.add(w([x, y - 1]));
    }
  }
  step += 1;
}
console.log(step + 1);
