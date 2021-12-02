import { readFileSync } from "fs";
import { fileURLToPath } from "url";

function isMainModule() {
  return process.argv[1] === fileURLToPath(import.meta.url);
}

const targetTurn = 30000000;

if (isMainModule()) {
  main();
}

function main() {
  const sequence = String(readFileSync("./15-1.in")).split(",").map(Number);
  const positions = new Map();
  sequence.forEach((n, pos) => positions.set(n, pos));

  let pos = sequence.length;
  while (pos <= targetTurn) {
    let n = 0;
    const lastN = sequence[pos - 1];
    let lastPos = positions.get(lastN);
    if (lastPos != null && lastPos < pos - 1) {
      n = pos - lastPos - 1;
    }
    sequence[pos] = n;
    positions.set(lastN, pos - 1);
    pos += 1;
  }
  console.log(sequence[targetTurn - 1]);
}
