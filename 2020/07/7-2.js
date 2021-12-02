const { readFileSync } = require("fs");

const rules = {};
String(readFileSync("./7-1.in"))
  .split("\n")
  .map((line) => {
    const [parentColor, childrenStr] = line.split(" bags contain ");
    rules[parentColor] = {};
    Array.from(childrenStr.matchAll(/(\d+) (.*?) bag/g)).map(
      ([, count, childColor]) => {
        rules[parentColor][childColor] = Number(count);
      }
    );
  });

function countBags(targetColor) {
  return Object.entries(rules[targetColor]).reduce(
    (acc, [childColor, count]) => {
      return acc + count + count * countBags(childColor);
    },
    0
  );
}

console.log(countBags("shiny gold"));
