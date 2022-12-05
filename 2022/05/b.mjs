import fs from "fs";

class State {
  static parse(str) {
    const state = [];
    str
      .split("\n")
      .slice(0, -1)
      .reverse()
      .forEach((row) => {
        Array.from(row.matchAll(/.(.). ?/g)).forEach(([, crate], pos) => {
          if (!state[pos]) {
            state[pos] = [];
          }
          if (crate !== " ") {
            state[pos].push(crate);
          }
        });
      });
    return new State(state);
  }

  constructor(state) {
    this.state = state;
  }

  move(move, from, to) {
    this.state[to - 1].push(...this.state[from - 1].slice(-move));
    this.state[from - 1] = this.state[from - 1].slice(
      0,
      this.state[from - 1].length - move
    );
  }
}

const [stateString, movesString] = fs
  .readFileSync("in")
  .toString()
  .trimEnd()
  .split("\n\n");
const state = State.parse(stateString);
movesString.split("\n").forEach((line) => {
  const [, move, from, to] = line.match(/move (\d+) from (\d) to (\d)/);
  state.move(Number(move), Number(from), Number(to));
});

console.log(state.state.map((col) => col.pop()).join(""));
