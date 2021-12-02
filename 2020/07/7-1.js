const { readFileSync } = require("fs");

const rules = {};
String(readFileSync("./7-1.in"))
  .split("\n")
  .map((line) => {
    const [parentColor, childrenStr] = line.split(" bags contain ");
    rules[parentColor] = {};
    Array.from(childrenStr.matchAll(/(\d+) (.*?) bag/g)).map(
      ([, count, childColor]) => {
        rules[parentColor][childColor] = count;
      }
    );
  });

function countPossibleContainer(targetColor) {
  let result = 0;
  let enqueledColors = [targetColor];
  let targetColors = [];
  const skippedColors = [targetColor];
  while (enqueledColors.length) {
    targetColors = enqueledColors;
    enqueledColors = [];
    Object.entries(rules).forEach(([parentColor, children]) => {
      if (!skippedColors.includes(parentColor)) {
        const couldContainSomeTargetColors = Object.keys(children).some(
          (childColor) => targetColors.includes(childColor)
        );
        if (couldContainSomeTargetColors) {
          result += 1;
          enqueledColors.push(parentColor);
          skippedColors.push(parentColor);
        }
      }
    });
  }
  return result;
}

console.log(countPossibleContainer("shiny gold"));
