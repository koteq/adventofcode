import fs from "fs";

const signal = fs.readFileSync("in").toString().trim();

const markerLength = 4;
for (let i = 0, lim = signal.length - markerLength; i < lim; i++) {
  if (new Set(signal.slice(i, i + markerLength)).size === markerLength) {
    console.log(i + markerLength);
    break;
  }
}
