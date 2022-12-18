import fs from "fs";

const data = fs.readFileSync("in").toString().trim().split("\n");
const solid = new Set(data);

const min = [
  Number.POSITIVE_INFINITY,
  Number.POSITIVE_INFINITY,
  Number.POSITIVE_INFINITY,
];
const max = [
  Number.NEGATIVE_INFINITY,
  Number.NEGATIVE_INFINITY,
  Number.NEGATIVE_INFINITY,
];
for (const line of solid) {
  const [x, y, z] = line.split(",").map(Number);
  min[0] = Math.min(min[0], x);
  min[1] = Math.min(min[1], y);
  min[2] = Math.min(min[2], z);
  max[0] = Math.max(max[0], x);
  max[1] = Math.max(max[1], y);
  max[2] = Math.max(max[2], z);
}
console.log({ min, max });

const fill = new Set(data);
const queue = new Set([[min[0] - 1, min[1] - 1, min[2] - 1].join(",")]);
while (queue.size) {
  const coords = queue[Symbol.iterator]().next().value;
  queue.delete(coords);
  fill.add(coords);
  const [x, y, z] = coords.split(",").map(Number);
  [
    [x + 1, y, z],
    [x, y + 1, z],
    [x, y, z + 1],
    [x - 1, y, z],
    [x, y - 1, z],
    [x, y, z - 1],
  ].forEach(([x, y, z]) => {
    const coords = [x, y, z].join(",");
    if (
      !fill.has(coords) &&
      !queue.has(coords) &&
      x <= max[0] + 1 &&
      y <= max[1] + 1 &&
      z <= max[2] + 1 &&
      x >= min[0] - 1 &&
      y >= min[1] - 1 &&
      z >= min[2] - 1
    ) {
      queue.add(coords);
    }
  });
  // console.log(fill.size);
}
console.log("Fill", fill.size);

const air = new Set();
for (let x = min[0] - 1; x <= max[0] + 1; x++) {
  for (let y = min[1] - 1; y <= max[1] + 1; y++) {
    for (let z = min[2] - 1; z <= max[2] + 1; z++) {
      const coords = [x, y, z].join(",");
      if (!fill.has(coords)) air.add(coords);
    }
  }
}
console.log("Air", air.size);

let result = 0;
for (const line of solid) {
  const [x, y, z] = line.split(",").map(Number);
  const adjacent = [
    [x + 1, y + 0, z + 0],
    [x - 1, y + 0, z + 0],
    [x + 0, y + 1, z + 0],
    [x + 0, y - 1, z + 0],
    [x + 0, y + 0, z + 1],
    [x + 0, y + 0, z - 1],
  ]
    .map((coords) => (solid.has(coords.join(",")) ? 1 : 0))
    .reduce((acc, n) => acc + n, 0);

  result += 6 - adjacent;
}

for (const line of air) {
  const [x, y, z] = line.split(",").map(Number);
  const adjacent = [
    [x + 1, y + 0, z + 0],
    [x - 1, y + 0, z + 0],
    [x + 0, y + 1, z + 0],
    [x + 0, y - 1, z + 0],
    [x + 0, y + 0, z + 1],
    [x + 0, y + 0, z - 1],
  ]
    .map((coords) => (air.has(coords.join(",")) ? 1 : 0))
    .reduce((acc, n) => acc + n, 0);

  result -= 6 - adjacent;
}
console.log(result);
