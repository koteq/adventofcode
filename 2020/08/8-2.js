const { readFileSync } = require("fs");

const prog = String(readFileSync("./8-1.in"))
  .split("\n")
  .map((l) => {
    let [op, val] = l.split(" ");
    return [op, Number(val)];
  });

function run(patchPos) {
  const handlers = {
    nop: () => (pos += 1),
    acc: (val) => ((acc += val), (pos += 1)),
    jmp: (val) => (pos += val),
  };

  let pos = 0;
  let acc = 0;
  let opOfInterestIterator = 0;
  const visited = new Set();
  while (true) {
    let [op, val] = prog[pos];
    if (op === "nop" || op === "jmp") {
      opOfInterestIterator += 1;
      if (opOfInterestIterator === patchPos) {
        op = op === "nop" ? "jmp" : "nop";
      }
    }
    visited.add(pos);
    handlers[op](val);
    if (visited.has(pos)) {
      return;
    }
    if (pos >= prog.length) {
      console.log(acc);
      process.exit(0);
    }
  }
}

for (let i = 1; i < prog.length; i++) {
  run(i);
}
