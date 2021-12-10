import fs from "fs";

const lines = fs.readFileSync("in").toString().trim().split("\n");

const isOpening = (brace) => "([{<".includes(brace);
const isClosing = (brace) => ">}])".includes(brace);
const isMatching = (a, b) =>
  (a === "(" && b === ")") ||
  (a === "[" && b === "]") ||
  (a === "{" && b === "}") ||
  (a === "<" && b === ">");

const scores = [];
const pointsByBrace = {
  "(": 1,
  "[": 2,
  "{": 3,
  "<": 4,
};
for (const line of lines) {
  calculateLineScore: {
    const stack = [];
    for (const brace of line) {
      if (isOpening(brace)) {
        stack.push(brace);
      } else {
        const closing = stack.pop();
        if (!isMatching(closing, brace)) {
          break calculateLineScore;
        }
      }
    }
    const lineScore = stack
      .reverse()
      .reduce((acc, brace) => acc * 5 + pointsByBrace[brace], 0);
    scores.push(lineScore);
  }
}

console.log(scores.sort((a, b) => a - b));
console.log(scores.sort((a, b) => a - b)[Math.floor(scores.length / 2)]);
