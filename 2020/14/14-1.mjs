import { readFileSync } from "fs";

let mask = "";
const memory = new Map();
String(readFileSync("./14-1.in"))
  .split("\n")
  .forEach((line) => {
    if (line.startsWith("mask")) {
      [, mask] = line.split(" = ");
    } else {
      const [, address, value] = line.match(/(\d+)\D+(\d+)/);
      memory.set(address, applyMask(BigInt(value), mask));
    }
  });

const sum = Array.from(memory.values()).reduce((acc, cur) => acc + cur, 0n);
console.log(sum);

function applyMask(value, mask) {
  let result = value;
  Array.from(mask)
    .map(Number)
    .forEach((bit, pos) => {
      if (Number.isFinite(bit)) {
        const offset = BigInt(mask.length - pos - 1);
        if (bit) {
          result |= 1n << offset;
        } else {
          result &= ~(1n << offset);
        }
      }
    });
  return result;
}
