const { readFileSync } = require("fs");

function uniqCount(str) {
  let result = [];
  const answers = str.split("\n");
  new Set(Array.from(str.matchAll(/[a-z]/g)).map(([c]) => c)).forEach((c) => {
    if (answers.every((l) => l.includes(c))) {
      result.push(c);
    }
  });
  return result.length;
}

const sum = String(readFileSync("./6-1.in"))
  .split("\n\n")
  .map(uniqCount)
  .reduce((acc, i) => acc + i, 0);
console.log(sum);
