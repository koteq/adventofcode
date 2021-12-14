import fs from "fs";

let template;
const pairs = new Map();
{
  const [templateStr, pairsStr] = fs
    .readFileSync("in")
    .toString()
    .trim()
    .split("\n\n");
  template = templateStr.split("");
  pairsStr.split("\n").forEach((line) => pairs.set(...line.split(" -> ")));
}

// process polymer
for (let step = 0; step < 10; step++) {
  let pos = 0;
  while (pos < template.length - 1) {
    const pair = template.slice(pos, pos + 2).join("");
    const insertion = pairs.get(pair);
    template.splice(pos + 1, 0, insertion);
    pos += 2;
  }
}

const counters = {};
for (const char of template) {
  counters[char] = (counters[char] ?? 0) + 1;
}

const sorted = Object.values(counters).sort((a, b) => a - b);
const leastCommon = sorted[0];
const mostCommon = sorted[sorted.length - 1];
console.log(mostCommon - leastCommon);
