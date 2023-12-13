import fs from "node:fs";

function main(inputFile = "in") {
  const data = fs.readFileSync(inputFile).toString().trim().split("\n\n");
  const patterns = data.map((data) => new Pattern(data));
  const answer = patterns.reduce(
    (acc, p) => acc + p.vertical + 100 * p.horizontal,
    0
  );
  console.log(answer);
}

class Pattern {
  constructor(data) {
    this.width = data.indexOf("\n");
    data = data.replaceAll("\n", "");
    this.height = Math.ceil(data.length / this.width);
    this.rows = [];
    this.cols = [];
    for (let y = 0; y < this.height; y++) {
      let row = 0;
      for (let x = 0; x < this.width; x++) {
        const char = data[x + y * this.width];
        if (char === "#") {
          row += 1 << (this.width - x - 1);
        }
      }
      this.rows.push(row);
    }
    for (let x = 0; x < this.width; x++) {
      let col = 0;
      for (let y = 0; y < this.height; y++) {
        const char = data[x + y * this.width];
        if (char === "#") {
          col += 1 << (this.height - y - 1);
        }
      }
      this.cols.push(col);
    }
  }

  get horizontal() {
    const middle = Math.ceil(this.height / 2);
    for (let offset = 0; offset < this.height - 1; offset++) {
      const pos = middle + Math.ceil(offset / 2) * (offset % 2 === 0 ? 1 : -1);
      let reflects = true;
      let smudgeFound = false;
      for (let i = 0, m = Math.min(pos, this.height - pos); i < m; i++) {
        if (this.rows[pos - i - 1] !== this.rows[pos + i]) {
          if (
            !smudgeFound &&
            mismatchBits(
              this.rows[pos - i - 1],
              this.rows[pos + i],
              this.width
            ) === 1
          ) {
            smudgeFound = true;
          } else {
            reflects = false;
            break;
          }
        }
      }
      if (reflects && smudgeFound) {
        return pos;
      }
    }
    return 0;
  }

  get vertical() {
    const middle = Math.ceil(this.width / 2);
    for (let offset = 0; offset < this.width - 1; offset++) {
      const pos = middle + Math.ceil(offset / 2) * (offset % 2 === 0 ? 1 : -1);
      let reflects = true;
      let smudgeFound = false;
      for (let i = 0, m = Math.min(pos, this.width - pos); i < m; i++) {
        if (this.cols[pos - i - 1] !== this.cols[pos + i]) {
          if (
            !smudgeFound &&
            mismatchBits(
              this.cols[pos - i - 1],
              this.cols[pos + i],
              this.height
            ) === 1
          ) {
            smudgeFound = true;
          } else {
            reflects = false;
            break;
          }
        }
      }
      if (reflects && smudgeFound) {
        return pos;
      }
    }
    return 0;
  }
}

function mismatchBits(a, b, l) {
  let cnt = 0;
  const xor = a ^ b;
  for (let i = 0; i < l; i++) {
    if ((xor >> i) & 1) {
      cnt += 1;
    }
  }
  return cnt;
}

main();
