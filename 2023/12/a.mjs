import fs from "node:fs";

function main(inputFile = "in") {
  // len -> rule -> [[mask, mask], ...]
  const map = new Map();
  const data = fs
    .readFileSync(inputFile)
    .toString()
    .trim()
    .split("\n")
    .map((line) => line.split(" "));
  for (const [row, rule] of data) {
    const len = row.length;
    if (!map.has(len)) {
      map.set(len, new Map());
    }
    if (!map.get(len).has(rule)) {
      map.get(len).set(rule, []);
    }
    map.get(len).get(rule).push(toMasks(row));
  }

  let answer = 0;
  for (const len of map.keys()) {
    for (let i = 0, lim = 2 ** len; i < lim; i++) {
      const rule = toRule(i, len);
      const rows = map.get(len).get(rule);
      if (rows) {
        for (const [opMask, dmgMask] of rows) {
          if ((i & opMask) === opMask && (~i & dmgMask) === dmgMask) {
            answer += 1;
          }
        }
      }
    }
  }

  console.log(answer);
}

function toMasks(row) {
  const len = row.length;
  let opMask = 0;
  let dmgMask = 0;
  for (let i = 0; i < len; i++) {
    if (row[len - i - 1] === ".") {
      opMask += 1 << i;
    } else if (row[len - i - 1] === "#") {
      dmgMask += 1 << i;
    }
  }
  return [opMask, dmgMask];
}

function toRule(num, len) {
  let cnt = 0;
  const rule = [];
  for (let i = 0; i < len; i++) {
    const bit = num & 1;
    if (bit) {
      if (cnt) {
        rule.push(cnt);
        cnt = 0;
      }
    } else {
      cnt += 1;
    }
    num >>= 1;
  }
  if (cnt) {
    rule.push(cnt);
    cnt = 0;
  }
  rule.reverse();
  return rule.join(",");
}

main();
