import fs from "fs";

let img = new Map();
let lim = { t: Infinity, r: -Infinity, b: -Infinity, l: Infinity };
const enhancement = [];
{
  const [enhancementData, imageData] = fs
    .readFileSync("in")
    .toString()
    .trim()
    .split("\n\n");
  enhancementData
    .split("")
    .forEach((char) => enhancement.push(char === "#" ? true : false));
  imageData.split("\n").forEach((line, y) =>
    line.split("").forEach((char, x) => {
      if (char === "#") {
        img.set([x, y].toString(), true);
        lim.l = Math.min(lim.l, x);
        lim.r = Math.max(lim.r, x);
        lim.t = Math.min(lim.t, y);
        lim.b = Math.max(lim.b, y);
      }
    })
  );
}

console.log(img.size);
for (let i = 0; i < 2; i++) {
  const src = new Map(img);
  img = new Map();
  const nLim = { ...lim };
  for (let y = lim.t - 4; y < lim.b + 4; y++) {
    for (let x = lim.l - 4; x < lim.r + 4; x++) {
      const adjacent = [
        [x - 1, y - 1],
        [x, y - 1],
        [x + 1, y - 1],
        [x - 1, y],
        [x, y],
        [x + 1, y],
        [x - 1, y + 1],
        [x, y + 1],
        [x + 1, y + 1],
      ];
      const bin = adjacent
        .map((coord) => (src.has(coord.toString()) ? "1" : "0"))
        .join("");
      const enhIndex = parseInt(bin, 2);
      if (enhancement[enhIndex]) {
        img.set([x, y].toString(), true);
        nLim.l = Math.min(nLim.l, x);
        nLim.r = Math.max(nLim.r, x);
        nLim.t = Math.min(nLim.t, y);
        nLim.b = Math.max(nLim.b, y);
      }
    }
  }
  if (i % 2) {
    lim = nLim;
  } else {
    lim.t += 1;
    lim.r -= 1;
    lim.b -= 1;
    lim.l += 1;
  }
  console.log(img.size);
}
