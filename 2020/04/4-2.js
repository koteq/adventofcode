const { readFileSync } = require("fs");

const input = String(readFileSync("./4-1.in")).split("\n\n");

const year = (lo, hi) => (str) => /^\d{4}$/.test(str) && number(lo, hi)(str);
const number = (lo, hi) => (str) => lo <= Number(str) && Number(str) <= hi;
const height = (str) => {
  const rules = {
    cm: number(150, 193),
    in: number(59, 76),
  };
  const match = str.match(/^(\d+)(cm|in)$/);
  if (match) {
    const [, height, system] = match;
    const validator = rules[system];
    if (validator) {
      return validator(height);
    }
  }
  return false;
};
const hexColor = (str) => /^#[0-9a-f]{6}$/.test(str);
const eyeColor = (str) =>
  ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"].includes(str);
const passportId = (str) => /^\d{9}$/.test(str);

const validationRules = {
  byr: year(1920, 2002),
  iyr: year(2010, 2020),
  eyr: year(2020, 2030),
  hgt: height,
  hcl: hexColor,
  ecl: eyeColor,
  pid: passportId,
  // 'cid',
};
let validCount = 0;

input.forEach((l) => {
  const passport = {};
  Array.from(l.matchAll(/(\w{3}):(\S+)/g)).forEach(([, k, v]) => {
    passport[k] = v;
  });
  const isValid = Object.entries(validationRules).every(([key, rule]) => {
    return passport[key] && rule(passport[key]);
  });
  if (isValid) {
    validCount += 1;
  }
});

console.log(validCount);
