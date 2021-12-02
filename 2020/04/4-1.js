const { readFileSync } = require("fs");

const input = String(readFileSync("./4-1.in")).split("\n\n");
const requiredKeys = [
  "byr",
  "iyr",
  "eyr",
  "hgt",
  "hcl",
  "ecl",
  "pid",
  // 'cid',
];
let validCount = 0;

input.forEach((l) => {
  const passport = {};
  Array.from(l.matchAll(/(\w{3}):(\S+)/g)).forEach(([, k, v]) => {
    passport[k] = v;
  });
  if (requiredKeys.every((k) => passport[k])) {
    validCount += 1;
  }
});

console.log(validCount);
