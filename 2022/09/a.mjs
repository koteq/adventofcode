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
    this.headPos = new Pos(0, 0);
    this.tailPos = new Pos(0, 0);
  }

  moveHead(direction, steps) {
    this.headPos.move(direction, steps);
    if (Math.abs(this.headPos.x - this.tailPos.x) > 1) {
      const offset = this.headPos.x - this.tailPos.x;
      this.tailPos.x = this.headPos.x - offset / Math.abs(offset);
      if (Math.abs(this.headPos.y - this.tailPos.y) === 1) {
        this.tailPos.y = this.headPos.y;
      }
    }
    if (Math.abs(this.headPos.y - this.tailPos.y) > 1) {
      const offset = this.headPos.y - this.tailPos.y;
      this.tailPos.y = this.headPos.y - offset / Math.abs(offset);
      if (Math.abs(this.headPos.x - this.tailPos.x) === 1) {
        this.tailPos.x = this.headPos.x;
      }
    }
  }

  print() {
    const h = 5;
    const w = 6;
    const state = Array.from({ length: h }).map(() =>
      Array.from({ length: w }).map(() => ".")
    );
    state[h - 0 - 1][0] = "s";
    state[h - this.tailPos.y - 1][this.tailPos.x] = "T";
    state[h - this.headPos.y - 1][this.headPos.x] = "H";
    console.log(state.map((line) => line.join("")).join("\n") + "\n");
  }
}

const rope = new Rope();
const tailVisited = new Map();
for (const [direction, steps] of motions) {
  for (let i = 0; i < steps; i++) {
    rope.moveHead(direction, 1);
    // rope.print();
    tailVisited.set(`${rope.tailPos.x},${rope.tailPos.y}`, true);
  }
}
console.log(tailVisited.size);
