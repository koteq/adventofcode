import fs from "fs";

const reg = {
  w: 0,
  x: 0,
  y: 0,
  z: 0,
};
function v(v) {
  if (/[wxyz]/.test(v)) {
    return reg[v];
  } else {
    return Number(v);
  }
}
const ops = {
  inp: ([a]) => (reg[a] = 9), // TODO 1..9
  add: ([a, b]) => (reg[a] += v(b)),
  mul: ([a, b]) => (reg[a] *= v(b)),
  div: ([a, b]) => {
    const bVal = v(b);
    if (bVal === 0) {
      console.log();
    }
    reg[a] = Math.round(reg[a] / bVal);
  },
  mod: ([a, b]) => {
    const bVal = v(b);
    if (reg[a] < 0 || bVal <= 0) {
      console.log();
    }
    reg[a] = reg[a] % bVal;
  },
  eql: ([a, b]) => (reg[a] = reg[a] === v(b) ? 1 : 0),
};
function run(instruction) {
  const [op, ...rest] = instruction.split(" ");
  ops[op](rest);
}
const monad = fs.readFileSync("in").toString().trim().split("\n");
const r1 = [];
const r2 = [];
monad.forEach((instruction, id) => {
  run(instruction);
  if (id % 18 === 5) {
    const [op, a, b] = instruction.split(" ");
    r1.push(Number(b));
  }
  if (id % 18 === 15) {
    const [op, a, b] = instruction.split(" ");
    r2.push(Number(b));
  }
});
console.log(reg);
// console.log(r1, r2);
