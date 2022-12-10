import fs from "fs";

const poi = [20, 60, 100, 140, 180, 220];
const state = {
  X: 1,
  cycle: 0,
  strength: 0,
  __proto__: {
    _inc() {
      this.cycle += 1;
      if (poi.includes(this.cycle)) {
        this.strength += this.cycle * this.X;
      }
    },
    noop() {
      this._inc();
    },
    addx(val) {
      this._inc();
      this._inc();
      this.X += val;
    },
  },
};

fs.readFileSync("in")
  .toString()
  .trim()
  .split("\n")
  .forEach((line) => {
    if (line === "noop") {
      state.noop();
    } else {
      const val = Number(line.split(" ")[1]);
      state.addx(val);
    }
  });
console.log(state.strength);
