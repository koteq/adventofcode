const { readFileSync } = require("fs");

const prog = String(readFileSync("./8-1.in"))
  .split("\n")
  .map((l) => {
    let [op, val] = l.split(" ");
    return [op, Number(val)];
  });

const handlers = {
  nop: () => (pos += 1),
  acc: (val) => ((acc += val), (pos += 1)),
  jmp: (val) => (pos += val),
};

let pos = 0;
let acc = 0;
const visited = new Set();
while (true) {
  const [op, val] = prog[pos];
  visited.add(pos);
  handlers[op](val);
  if (visited.has(pos)) {
    console.log(acc);
    process.exit(1);
  }
}
