import fs from "node:fs";

function main(inputFile = "in") {
  const map = new Map();
  const rows = fs
    .readFileSync(inputFile)
    .toString()
    .trim()
    .split("\n")
    .map((line) => new DivideAndConquer(line));

  const answer = rows.reduce((acc, row) => {
    console.log(`Calculating arrangements for ${row.line}`);
    const arrangements = row.getArrangements();
    return acc + arrangements;
  }, 0);

  console.log(answer);
}

class DivideAndConquer {
  constructor(line) {
    this.line = line;
    const [pattern, rule] = line.split(" ");
    this.pattern = `${pattern}?${pattern}?${pattern}?${pattern}?${pattern}`;
    this.rule = `${rule},${rule},${rule},${rule},${rule}`
      .split(",")
      .map(Number);
    this.dp1 = new Map();
    this.dp2 = new Map();
  }

  getArrangements() {
    console.log(`  split`);
    const arrangementsArr = [];
    for (const groupLength of this.rule) {
      const arrangements = new Arrangements();
      for (const [l, r] of this.getSimpleArrangementRanges(groupLength)) {
        arrangements.push(l, r, 1);
      }
      arrangementsArr.push(arrangements);
    }
    console.log(`  merge`);
    let mergedArrangements = arrangementsArr.at(0);
    for (let i = 1; i < arrangementsArr.length; i++) {
      mergedArrangements = this.mergeArrangements(
        mergedArrangements,
        arrangementsArr.at(i)
      );
    }
    console.log(`  verify`);
    const filtered = mergedArrangements.data.filter((a) =>
      this.verifyArrangement(a)
    );
    const ttlCnt = filtered.reduce((acc, [l, r, c]) => acc + c, 0);
    return ttlCnt;
  }

  verifyArrangement([l, r]) {
    for (let i = 0; i < l; i++) {
      if (this.pattern.at(i) === "#") {
        return false;
      }
    }
    for (let i = r; i < this.pattern.length; i++) {
      if (this.pattern.at(i) === "#") {
        return false;
      }
    }
    return true;
  }

  mergeArrangements(a, b) {
    const arrangements = new Arrangements();
    for (const [al, ar, ac] of a.data) {
      for (const [bl, br, bc] of b.data) {
        if (ar < bl && this.verifyNoBrokenSprings(ar, bl)) {
          arrangements.push(al, br, ac * bc);
        }
      }
    }
    return arrangements;
  }

  verifyNoBrokenSprings(l, r) {
    const key = `${l}:${r}`;
    if (!this.dp2.has(key)) {
      this.dp2.set(key, !this.pattern.slice(l, r).includes("#"));
    }
    return this.dp2.get(key);
  }

  getSimpleArrangementRanges(groupLength) {
    if (!this.dp1.has(groupLength)) {
      const arrangements = [];
      for (let i = 0; i <= this.pattern.length - groupLength; i++) {
        if (this.matchGroup(i, groupLength)) {
          arrangements.push([i, i + groupLength]);
        }
      }
      this.dp1.set(groupLength, arrangements);
    }
    return this.dp1.get(groupLength);
  }

  matchGroup(offset, groupLength) {
    if (offset - 1 >= 0 && this.pattern.at(offset - 1) === "#") {
      return false;
    }
    for (let i = offset; i < offset + groupLength; i++) {
      if (this.pattern.at(i) === ".") {
        return false;
      }
    }
    if (
      offset + groupLength < this.pattern.length &&
      this.pattern.at(offset + groupLength) === "#"
    ) {
      return false;
    }
    return true;
  }
}

class Arrangements {
  constructor() {
    this.data = [];
    this.map = new Map();
  }

  push(l, r, c) {
    const key = `${l}:${r}`;
    const idx = this.map.get(key);
    if (idx !== undefined) {
      this.data[idx][2] += c;
    } else {
      this.data.push([l, r, c]);
      this.map.set(key, this.data.length - 1);
    }
  }

  at(idx) {
    return this.data.at(idx);
  }

  get length() {
    return this.data.length;
  }
}

main();
