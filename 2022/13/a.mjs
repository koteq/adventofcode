import fs from "fs";

function compare(_a, _b) {
  const a = Array.isArray(_a) ? _a : [_a];
  const b = Array.isArray(_b) ? _b : [_b];
  for (let i = 0, len = Math.min(a.length, b.length); i < len; i++) {
    if (Number.isFinite(a[i]) && Number.isFinite(b[i])) {
      if (a[i] < b[i]) return true;
      if (a[i] > b[i]) return false;
    } else {
      const result = compare(a[i], b[i]);
      if (result !== undefined) return result;
    }
  }
  if (a.length < b.length) return true;
  if (a.length > b.length) return false;
}

const tap = (arg) => {
  console.log(arg);
  return arg;
};

const result = fs
  .readFileSync("in")
  .toString()
  .trim()
  .split("\n\n")
  .map((pair) => pair.split("\n").map((line) => JSON.parse(line)))
  .map((pair) => compare(...pair))
	// .map(tap)
  .reduce((acc, cmp, idx) => (cmp ? acc + idx + 1 : acc), 0);
console.log(result);
