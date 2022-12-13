import fs from "fs";

function compare(_a, _b) {
  const a = Array.isArray(_a) ? _a : [_a];
  const b = Array.isArray(_b) ? _b : [_b];
  for (let i = 0, len = Math.min(a.length, b.length); i < len; i++) {
    if (Number.isFinite(a[i]) && Number.isFinite(b[i])) {
      if (a[i] < b[i]) return -1;
      if (a[i] > b[i]) return 1;
    } else {
      const result = compare(a[i], b[i]);
      if (result !== undefined) return result;
    }
  }
  if (a.length < b.length) return -1;
  if (a.length > b.length) return 1;
}

const tap = (arg) => {
  console.log(arg);
  return arg;
};

const packets = fs
  .readFileSync("in")
  .toString()
  .trim()
  .split("\n\n")
  .flatMap((pair) => pair.split("\n").map((line) => JSON.parse(line)));
packets.push([[2]]);
packets.push([[6]]);
packets.sort(compare);
const result = packets
  .map((p) => JSON.stringify(p))
  .reduce(
    (acc, p, idx) => acc * (p === "[[2]]" || p === "[[6]]" ? idx + 1 : 1),
    1
  );
console.log(result);
