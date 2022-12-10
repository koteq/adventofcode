import fs from "fs";

const h = 6;
const w = 40;
const screen = {
  X: 1,
  cycle: 0,
  pixels: Array.from({ length: h }).map(() => []),

  __proto__: {
    _draw() {
      this.cycle += 1;
      const x = (this.cycle - 1) % w;
      const y = Math.trunc((this.cycle - 1) / w);
      const shouldDraw = [this.X - 1, this.X, this.X + 1].includes(x);
      this.pixels[y][x] = shouldDraw ? "#" : ".";
    },
    noop() {
      this._draw();
    },
    addx(val) {
      this._draw();
      this._draw();
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
      screen.noop();
    } else {
      const val = Number(line.split(" ")[1]);
      screen.addx(val);
    }
  });
console.log(screen.pixels.map((line) => line.join("")).join("\n"));
