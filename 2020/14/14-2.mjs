import { readFileSync } from "fs";
import { fileURLToPath } from "url";

function isMainModule() {
  return process.argv[1] === fileURLToPath(import.meta.url);
}

if (isMainModule()) {
  main();
}

function main() {
  let mask = "";
  const memory = new Map();
  String(readFileSync("./14-1.in"))
    .split("\n")
    .forEach((line) => {
      if (line.startsWith("mask")) {
        [, mask] = line.split(" = ");
      } else {
        const [, address, value] = line.match(/(\d+)\D+(\d+)/);
        applyMask(BigInt(address), mask).forEach((address) => {
          memory.set(address, BigInt(value));
        });
      }
    });

  const sum = Array.from(memory.values()).reduce((acc, cur) => acc + cur, 0n);
  console.log(sum);
}

function applyMask(address, mask) {
  let result = [address];
  Array.from(mask).forEach((bit, pos) => {
    const offset = BigInt(mask.length - pos - 1);
    switch (bit) {
      case "0":
        // do nothing
        break;
      case "1":
        // replace address bit to 1
        result = result.map((address) => address | (1n << offset));
        break;
      case "X":
        // add both variations of the floating bit
        result = result.flatMap((address) => [
          address | (1n << offset),
          address & ~(1n << offset),
        ]);
        break;
    }
  });
  return result;
}
