import fs from "fs";

const data = fs
  .readFileSync("in")
  .toString()
  .trim()
  .split("\n")
  .map((line) =>
    line
      .split(" | ")
      .map(sequenceStr => sequenceStr.split(" "))
  );
let easyDigitsCount = 0;
for (const [signals, outputs] of data) {
  for (const output of outputs) {
    if ([2, 4, 3, 7].includes(output.length)) {
      easyDigitsCount += 1;
    }
  }
}
console.log(easyDigitsCount);
