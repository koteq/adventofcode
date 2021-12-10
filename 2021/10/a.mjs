import fs from "fs";

const lines = fs.readFileSync("in").toString().trim().split("\n");

const isOpening = (brace) => "([{<".includes(brace);
const isClosing = (brace) => ">}])".includes(brace);
const isMatching = (a, b) =>
  (a === "(" && b === ")") ||
  (a === "[" && b === "]") ||
  (a === "{" && b === "}") ||
  (a === "<" && b === ">");

let score = 0;
const pointsByBrace = {
  ")": 3,
  "]": 57,
  "}": 1197,
  ">": 25137,
};
for (const line of lines) {
  const stack = [];
  for (const brace of line) {
    if (isOpening(brace)) {
      stack.push(brace);
    } else {
      const closing = stack.pop();
      if (!isMatching(closing, brace)) {
        score += pointsByBrace[brace];
      }
    }
  }
}

console.log(score);
