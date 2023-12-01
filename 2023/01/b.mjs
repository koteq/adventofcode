import fs from "fs";

const lines = fs.readFileSync("in").toString().trim().split("\n");
const digits = {
  one: 1,
  two: 2,
  three: 3,
  four: 4,
  five: 5,
  six: 6,
  seven: 7,
  eight: 8,
  nine: 9,
};
const calibrationValues = lines.map((line) => {
  let [, first] = line.match(
    new RegExp(`(\\d|${Object.keys(digits).join("|")})`)
  );
  first = digits[first] ?? first;

  let [, last] = line.match(
    new RegExp(`.*(\\d|${Object.keys(digits).join("|")})`)
  );
  last = digits[last] ?? last;

  return Number(`${first}${last}`);
});
const calibration = calibrationValues.reduce((acc, val) => acc + val, 0);
console.log(calibration);
