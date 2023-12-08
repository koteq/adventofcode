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
  const ghosts = Array.from(nodes.values())
    .filter((node) => node.id.endsWith("A"))
    .map((start) => new Ghost(start));

  let steps = 0;
  do {
    const instruction = instructions.at(steps % instructions.length);
    ghosts.forEach((ghost) => ghost.navigate(instruction));
    steps += 1;
  } while (!ghosts.every((ghost) => ghost.period));
  const periods = ghosts.map((g) => g.period);

  let answer = periods.reduce(
    (acc, period) => (acc === 0 ? period : lcd(acc, period)),
    0
  );
  console.log(answer);
}

function gcd(a, b) {
  let gcd = 0;
  const min = Math.min(a, b);
  for (let d = 1; d <= min; d++) {
    if (a % d === 0 && b % d === 0) {
      gcd = d;
    }
  }
  return gcd;
}

function lcd(a, b) {
  const lcd = (a * b) / gcd(a, b);
  return lcd;
}

class Node {
  constructor(id, left, right) {
    this.id = id;
    this.left = left;
    this.right = right;
  }
}

class Ghost {
  constructor(start) {
    this.current = start;
    this.steps = 0;
    this.period = null;
  }

  navigate(instruction) {
    if (this.period) {
      return;
    }
    this.current = instruction === "L" ? this.current.left : this.current.right;
    this.steps += 1;
    if (this.current.id.endsWith("Z")) {
      this.period = this.steps;
    }
  }
}

main();
