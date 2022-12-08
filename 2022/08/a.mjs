import fs from "fs";

class Grid {
  constructor(data) {
    this.data = data;
    this.direction = 0;
    this.offset = 0;
  }

  [Symbol.iterator]() {
    return this;
  }

  next() {
    switch (this.direction) {
      case 0:
      case 1: {
        const line = Array.from(this.data[this.offset]).map(Number);
        const value = line.map((n, x) => [n, `${x}:${this.offset}`]);
        if (this.direction === 1) value.reverse();

        this.offset += 1;
        if (this.offset >= this.data.length) {
          this.offset = 0;
          this.direction += 1;
        }
        return { done: false, value };
      }
      case 2:
      case 3: {
        const line = this.data.map(l => l[this.offset]).map(Number);
        const value = line.map((n, y) => [n, `${this.offset}:${y}`]);
        if (this.direction === 3) value.reverse();

        this.offset += 1;
        if (this.offset >= this.data[0].length) {
          this.offset = 0;
          this.direction += 1;
        }
        return { done: false, value };
      }
      case 4:
        return { done: true };
    }
  }
}

const data = fs.readFileSync("in").toString().trim().split("\n");
const grid = new Grid(data);
const visible = new Map();
for (const line of grid) {
  let c = -1;
  line.forEach(([n, pos]) => {
    if (n > c) {
      c = n;
      visible.set(pos, true);
    }
  });
}
console.log(visible.size);
