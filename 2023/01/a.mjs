import fs from "fs";

const lines = fs.readFileSync("in").toString().trim().split("\n");
const calibrationValues = lines.map((line) => {
  const digits = line.replace(/[^\d]/g, "");
  return Number(`${digits.at(0)}${digits.at(-1)}`);
});
const calibration = calibrationValues.reduce((acc, val) => acc + val, 0);
console.log(calibration);
