import fs from "fs";

function getDuplicate([a, b]) {
  const occurrences = new Map();
  for (const char of a) {
    occurrences.set(char, (occurrences.get(char) ?? 0) + 1);
  }
  for (const char of b) {
    if (occurrences.has(char)) return char;
  }
}

function getPriority(char) {
  const code = char.charCodeAt(0);
  const lowercaseCodeOffset = 96;
  const uppercaseCodeOffset = 64;
  const uppercasePriorityOffset = 26;
  if (code > lowercaseCodeOffset) return code - lowercaseCodeOffset;
  if (code > uppercaseCodeOffset) return code - uppercaseCodeOffset + uppercasePriorityOffset;
  throw `Unexpected char "${char}" with code ${code}`;
}

function tap(arg) {
  console.log(arg);
  return arg;
}

const result = fs.readFileSync("in")
  .toString()
  .trim()
  .split("\n")
  .map((l) => [l.slice(0, l.length / 2), l.slice(l.length / 2)])
  .map(getDuplicate)
  .map(getPriority)
  // .map(tap)
  .reduce((acc, i) => acc + i, 0);
console.log(result);
