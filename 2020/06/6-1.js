const { readFileSync } = require("fs");

function uniqCount(str) {
  return new Set(Array.from(str.matchAll(/[a-z]/g)).map(([c]) => c)).size;
}

const sum = String(readFileSync("./6-1.in"))
  .split("\n\n")
  .map(uniqCount)
  .reduce((acc, i) => acc + i, 0);
console.log(sum);
