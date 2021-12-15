import fs from "fs";

const riskMap = fs
  .readFileSync("in")
  .toString()
  .trim()
  .split("\n")
  .map((line) => line.split("").map(Number));

const maxX = riskMap[0].length;
const maxY = riskMap.length;
for (let ry = 0; ry < 5; ry++) {
  for (let rx = 0; rx < 5; rx++) {
    for (let y = 0; y < maxY; y++) {
      for (let x = 0; x < maxX; x++) {
        riskMap[y + maxY * ry] ??= [];
        riskMap[y + maxY * ry][x + maxX * rx] =
          ((riskMap[y][x] + rx + ry - 1) % 9) + 1;
      }
    }
  }
}

// console.log(riskMap.map((row) => row.join("")).join("\n"));

class PriorityQueue {
  constructor() {
    this.heap = [];
    this.set = new Set();
  }

  // print(s) {
  //   let buff = `${s}:\n`;
  //   let heap = [...this.heap];
  //   for (let i = 1; heap.length; i *= 2) {
  //     buff += heap.slice(0, i).map(([p]) => p).join(",") + "\n";
  //     heap = heap.slice(i, heap.length);
  //   }
  //   console.log(buff);
  // }

  includes(data) {
    return this.set.has(JSON.stringify(data));
  }

  push(priority, data) {
    this.set.add(JSON.stringify(data));
    this.heap.push([priority, data]);
    this.siftUp(this.heap.length - 1);
    // this.print(`push ${priority}`);
  }

  pop() {
    const result = this.heap[0];
    const pop = this.heap.pop();
    if (this.heap.length) {
      this.heap[0] = pop;
      this.siftDown(0);
    }
    this.set.delete(JSON.stringify(result));
    // this.print(`pop ${result[0]}`);
    return result;
  }

  // // TODO by key?
  // changePriority(index, newPriority) {
  //   const oldPriority = this.heap[index];
  //   this.heap[index] = newPriority;
  //   if (newPriority > oldPriority) {
  //     this.siftUp(index);
  //   } else {
  //     this.siftDown(index);
  //   }
  // }

  swap(a, b) {
    [this.heap[a], this.heap[b]] = [this.heap[b], this.heap[a]];
  }

  siftUp(index) {
    let parentIndex = Math.ceil(index / 2) - 1;
    while (index > 0 && this.heap[parentIndex]?.[0] > this.heap[index]?.[0]) {
      this.swap(index, parentIndex);
      index = parentIndex;
      parentIndex = Math.ceil(index / 2) - 1;
    }
  }

  siftDown(index) {
    let swapIndex = index;
    const leftIndex = index * 2 + 1;
    if (this.heap[leftIndex]?.[0] < this.heap[swapIndex]?.[0]) {
      swapIndex = leftIndex;
    }
    const rightIndex = index * 2 + 2;
    if (this.heap[rightIndex]?.[0] < this.heap[swapIndex]?.[0]) {
      swapIndex = rightIndex;
    }
    if (index !== swapIndex) {
      this.swap(index, swapIndex);
      this.siftDown(swapIndex);
    }
  }
}

const start = [0, 0];
const target = [riskMap[0].length - 1, riskMap.length - 1];
const queue = new PriorityQueue();
queue.push(0, start);
const cameFrom = new Map();

const gScore = new Map();
const getGScore = (pos) => gScore.get(pos.join(",")) ?? Infinity;
const setGScore = (pos, score) => gScore.set(pos.join(","), score);
setGScore(start, 0);

const h = ([x, y]) => target[0] - x + (target[1] - y);
const fScore = new Map();
const setFScore = (pos, score) => fScore.set(pos.join(","), score);
setFScore(start, h(start));

while (queue.heap.length) {
  const [currentFScore, currentPos] = queue.pop();
  // console.log(path.length);
  // const currentPos = path[path.length - 1];
  const [cx, cy] = currentPos;
  if (cx === target[0] && cy === target[1]) {
    // reconstruct path
    let totalRisk = riskMap[target[1]][target[0]];
    let current = target.join(",");
    while (cameFrom.has(current)) {
      current = cameFrom.get(current);
      const [x, y] = current.split(",");
      totalRisk += riskMap[y]?.[x];
      // riskMap[y][x] = "·";
    }
    console.log(totalRisk - riskMap[0][0]);
    // console.log(path.join("-"));
    // console.log(riskMap.map((row) => row.join("")).join("\n"));
    break;
  }
  const adjacent = [
    [cx + 1, cy],
    [cx, cy + 1],
    [cx - 1, cy],
    [cx, cy - 1],
  ];
  adjacent.forEach(([x, y]) => {
    const moveRisk = riskMap[y]?.[x];
    const tentative_gScore = getGScore(currentPos) + moveRisk;
    if (tentative_gScore < getGScore([x, y])) {
      cameFrom.set([x, y].join(","), currentPos.join(","));
      setGScore([x, y], tentative_gScore);
      setFScore([x, y], tentative_gScore + h([x, y]));
      if (!queue.includes([x, y])) {
        queue.push(tentative_gScore + h([x, y]), [x, y]);
      }
    }

    // const isVisited = path.some(([px, py]) => px === x && py === y);

    // if (!isVisited && moveRisk != null) {
    //   const newPathRisk = pathRisk + riskMap[y][x];
    //   const estimatedCost = target[0] - x + (target[1] - y);
    //   const newPathCost = newPathRisk + estimatedCost;
    //   queue.push(newPathRisk, [
    //     newPathRisk,
    //     [...JSON.parse(JSON.stringify(path)), [x, y]],
    //   ]);
    // }
  });
}
