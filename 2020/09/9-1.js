const { readFileSync } = require("fs");

const preambleLen = 25;
const input = String(readFileSync("./9-1.in")).split("\n").map(Number);
for (let i = preambleLen; i < input.length; i++) {
  const number = input[i];
  const preamble = input.slice(i - preambleLen, i);
  const isValid = preamble.some((preambleNumber) => {
    const idx = preamble.indexOf(number - preambleNumber);
    return idx !== -1 && preamble[idx] !== preambleNumber;
  });
  if (!isValid) {
    console.log(number);
    process.exit(0);
  }
}
