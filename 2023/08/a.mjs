import fs from "node:fs";

function main(input_file = "in") {
  const [instructions, nodesData] = fs
    .readFileSync(input_file)
    .toString()
    .trim()
    .split("\n\n");
  const nodes = new Map();
  for (const line of nodesData.split("\n")) {
    const [id, left, right] = Array.from(line.matchAll(/\w+/g)).map(([m]) => m);
    nodes.set(id, new Node(id, left, right));
  }
  for (const node of nodes.values()) {
    node.left = nodes.get(node.left);
    node.right = nodes.get(node.right);
  }

  const start = "AAA";
  const goal = "ZZZ";
  let steps = 0;
  let current = nodes.get(start);
  do {
    const instruction = instructions.at(steps % instructions.length);
    current = instruction === "L" ? current.left : current.right;
    steps += 1;
  } while (current.id !== goal);
  console.log(steps);
}

class Node {
  constructor(id, left, right) {
    this.id = id;
    this.left = left;
    this.right = right;
  }
}

main();
