import { readFileSync } from "fs";

const input = readFileSync("in").toString().trim("\n").split("\n");

function findValue(data, mostCommon) {
  let copy = [...data];
  for (let pos = 0; pos < data[0].length; pos += 1) {
    const count = countBitsAt(copy, pos);
    const length = copy.length;
    const mostCommonBit = count >= length / 2 ? "1" : "0";
    const leastCommonBit = count >= length / 2 ? "0" : "1";
    const requiredBit = mostCommon ? mostCommonBit : leastCommonBit;

    copy = copy.filter((line) => line[pos] === requiredBit);
    if (copy.length === 1) {
      break;
    }
  }
  return copy[0];
}

function countBitsAt(data, pos) {
  let result = 0;
  data.forEach((line) => {
    result += Number(line[pos]);
  });
  return result;
}

const oxygen = findValue(input, true);
const co2 = findValue(input, false);

console.log({ oxygen, co2, result: parseInt(oxygen, 2) * parseInt(co2, 2) });
