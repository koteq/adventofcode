import { readFileSync } from "fs";

const input = String(readFileSync("./13-1.in")).split("\n");
const ids = input[1].split(",").map(Number);
const idsCount = ids.filter(Number.isFinite).length;
const largestId = Math.max(...ids.filter(Number.isFinite));
const offset = ids.indexOf(largestId);
const skip = new Set([largestId]);
console.log(largestId);

let i = 0;
let pos = 0;
let jump = largestId;
while (true) {
  i += 1;
  pos += jump;
  ids.forEach((id, idOffset) => {
    if (Number.isFinite(id) && !skip.has(id)) {
      if (pos % id === (id + (offset - idOffset)) % id) {
        skip.add(id);
        console.log(id);
        jump *= id;
      }
    }
  });
  if (skip.size === idsCount) {
    console.log("Done", i, pos - offset);
    process.exit(0);
  }
}
