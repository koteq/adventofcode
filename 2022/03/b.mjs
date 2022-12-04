import fs from "fs";

function getDuplicate(lines) {
  const occurrences = new Map();
  lines
    .map((line) => new Set(Array.from(line)))
    .forEach((set) =>
      set.forEach((char) =>
        occurrences.set(char, (occurrences.get(char) ?? 0) + 1)
      )
    );
  for (const [k, v] of occurrences) {
    if (v === lines.length) {
      return k;
    }
  }
}

function getPriority(char) {
  const code = char.charCodeAt(0);
  const lowercaseCodeOffset = 96;
  const uppercaseCodeOffset = 64;
  const uppercasePriorityOffset = 26;
  if (code > lowercaseCodeOffset) return code - lowercaseCodeOffset;
  if (code > uppercaseCodeOffset)
    return code - uppercaseCodeOffset + uppercasePriorityOffset;
  throw `Unexpected char "${char}" with code ${code}`;
}

function tap(arg) {
  console.log(arg);
  return arg;
}

const result = fs
  .readFileSync("in")
  .toString()
  .trim()
  .split("\n")
  .reduce(
    (acc, l) => {
      if (acc[acc.length - 1].length >= 3) {
        acc.push([]);
      }
      acc[acc.length - 1].push(l);
      return acc;
    },
    [[]]
  )
  .map(getDuplicate)
  // .map(tap)
  .map(getPriority)
  .reduce((acc, i) => acc + i, 0);
console.log(result);
