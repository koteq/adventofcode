import fs from "fs";

const motions = fs
  .readFileSync("in")
  .toString()
  .trim()
  .split("\n")
  .map((l) => {
    const [direction, steps] = l.split(" ");
    return [direction, Number(steps)];
  });

const getMove = (direction, steps) =>
  ({
    L: [steps * -1, 0],
    R: [steps * +1, 0],
    U: [0, steps * +1],
    D: [0, steps * -1],
  }[direction]);

class Pos {
  constructor(x, y) {
    this.x = x;
    this.y = y;
  }

  move(direction, steps) {
    const [x, y] = getMove(direction, steps);
    this.x += x;
    this.y += y;
  }

  equals(pos) {
    return this.x === pos.x && this.y === pos.y;
  }

  dist(pos) {
    return Math.abs(this.x - pos.x) + Math.abs(this.y - pos.y);
  }
}

class Rope {
  constructor() {
    this.knots = Array.from({ length: 10 }).map((_, i) => new Pos(0, 0, i));
  }

  moveHead(direction, steps) {
    this.knots[0].move(direction, steps);
    let prev = this.knots[0];
    for (const knot of this.knots.slice(1, this.knots.length)) {
      if (
        Math.abs(prev.x - knot.x) > 1 &&
        Math.abs(prev.y - knot.y) > 1 /* &&
        Math.abs(prev.x - knot.x) === Math.abs(prev.y - knot.y) */
      ) {
        const offset1 = prev.x - knot.x;
        const offset2 = prev.y - knot.y;
        knot.x = prev.x - offset1 / Math.abs(offset1);
        knot.y = prev.y - offset2 / Math.abs(offset2);
      }
      if (Math.abs(prev.x - knot.x) > 1) {
        const offset = prev.x - knot.x;
        knot.x = prev.x - offset / Math.abs(offset);
        if (Math.abs(prev.y - knot.y) === 1) {
          knot.y = prev.y;
        }
      }
      if (Math.abs(prev.y - knot.y) > 1) {
        const offset = prev.y - knot.y;
        knot.y = prev.y - offset / Math.abs(offset);
        if (Math.abs(prev.x - knot.x) === 1) {
          knot.x = prev.x;
        }
      }
      prev = knot;
    }
  }

  print() {
    const ox = 11;
    const oy = -5;
    const h = 21;
    const w = 26;
    const state = Array.from({ length: h }).map(() =>
      Array.from({ length: w }).map(() => ".")
    );
    this.knots.forEach((knot, i) => {
      if (state[oy + h - knot.y - 1][ox + knot.x] === ".") {
        state[oy + h - knot.y - 1][ox + knot.x] = i === 0 ? "H" : i;
      }
    });
    if (state[oy + h - 0 - 1][ox + 0] === ".") {
      state[oy + h - 0 - 1][ox + 0] = "s";
    }
    console.log(state.map((line) => line.join("")).join("\n") + "\n");
  }
}

const rope = new Rope();
const tailVisited = new Map();
for (const [direction, steps] of motions) {
  // console.log(`\n== ${direction} ${steps} ==\n`);
  for (let i = 0; i < steps; i++) {
    rope.moveHead(direction, 1);
    const tail = rope.knots[rope.knots.length - 1];
    tailVisited.set(`${tail.x},${tail.y}`, true);
  }
  // rope.print();
}
console.log(tailVisited.size);
