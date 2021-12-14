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

function brute(template, steps) {
  template = [...template];
  for (let step = 0; step < steps; step++) {
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

  return counters;
}

function calculate(counters) {
  const sorted = Object.values(counters).sort((a, b) => a - b);
  const leastCommon = sorted[0];
  const mostCommon = sorted[sorted.length - 1];

  return mostCommon - leastCommon;
}

function smart(template, steps) {
  const pairCounts = new Map();
  const increment = (pair, amount = 1) =>
    pairCounts.set(pair, (pairCounts.get(pair) ?? 0) + amount);
  const decrement = (pair, amount = 1) =>
    pairCounts.set(pair, (pairCounts.get(pair) ?? 0) - amount);

  for (let pos = 0; pos < template.length - 1; pos++) {
    const pair = template.slice(pos, pos + 2).join("");
    increment(pair);
  }

  for (let step = 0; step < steps; step++) {
    new Map(pairCounts).forEach((count, pair) => {
      if (count > 0) {
        const [a, b] = pair;
        const insertion = pairs.get(pair);
        decrement(pair, count);
        increment(`${a}${insertion}`, count);
        increment(`${insertion}${b}`, count);
      }
    });
  }

  const countersA = {};
  const countersB = {};
  pairCounts.forEach((count, pair) => {
    const [a, b] = pair;
    countersA[a] = (countersA[a] ?? 0) + count;
    countersB[b] = (countersB[b] ?? 0) + count;
  });
  const counters = {};
  for (const [char, count] of [
    ...Object.entries(countersA),
    ...Object.entries(countersB),
  ]) {
    counters[char] = Math.max(counters[char] ?? 0, count);
  }

  return counters;
}

// console.log(calculate(brute(template, 10)));
console.log(calculate(smart(template, 40)));
