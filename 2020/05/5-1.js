const { readFileSync } = require("fs");

const rowBits = 7;
const colBits = 3;

function toTicket(str) {
  let row = 0;
  let col = 0;
  for (let i = 0; i < rowBits; i++) {
    const chr = str[i];
    if (chr === "B") {
      row += 1 << (rowBits - i - 1);
    }
  }
  for (let i = 0; i < colBits; i++) {
    const chr = str[i + rowBits];
    if (chr === "R") {
      col += 1 << (colBits - i - 1);
    }
  }
  return { row, col };
}

function calcId({ row, col }) {
  return row * 8 + col;
}

const ids = String(readFileSync("./5-1.in"))
  .split("\n")
  .map(toTicket)
  .map(calcId);
console.log(Math.max(...ids));
