import fs from "fs";
import Heap from "./heap.js";

class Vertex {
  constructor(options) {
    Object.assign(this, options);
  }
}

function getElevation(chr) {
  return chr.charCodeAt(0) - "a".charCodeAt(0);
}

let finish;
const heap = new Heap((a, b) => a.distance - b.distance);
const data = fs.readFileSync("in").toString().trim().split("\n");
const vertices = Array.from({ length: data.length }, () => []);
data.forEach((line, y) =>
  Array.from(line).forEach((chr, x) => {
    let elevation = getElevation(chr);
    let distance = Number.POSITIVE_INFINITY;
    if (chr === "S") {
      elevation = getElevation("a");
      distance = 0;
    } else if (chr === "E") {
      elevation = getElevation("z");
    }
    const V = new Vertex({ x, y, elevation, distance, previous: null });
    heap.push(V);
    vertices[y][x] = V;
    if (chr === "E") {
      finish = V;
    }
  })
);

function getUnvisitedNeighbors(U) {
  const neighbors = [
    vertices[U.y - 1]?.[U.x + 0],
    vertices[U.y + 1]?.[U.x + 0],
    vertices[U.y + 0]?.[U.x - 1],
    vertices[U.y + 0]?.[U.x + 1],
  ];
  const unvisitedReachableNeighbors = neighbors.filter(
    (V) => V && !V.visited && V.elevation - 1 <= U.elevation
  );
  return unvisitedReachableNeighbors;
}

function getEdgeDistance() {
  return 1;
}

while (!heap.empty()) {
  const U = heap.pop();
  U.visited = true;
  const neighbors = getUnvisitedNeighbors(U);
  neighbors.forEach((V) => {
    const newDistance = U.distance + getEdgeDistance(U, V);
    if (newDistance < V.distance) {
      V.distance = newDistance;
      V.previous = U;
      heap.updateItem(V);
    }
  });
}

let steps = 0;
let V = finish;
while (V.previous) {
  steps += 1
  V = V.previous;
}
console.log(steps);
// console.log(finish);
// console.log(
//   vertices
//     .map((line) => line.map((V) => (V.previous ? "#" : ".")).join(""))
//     .join("\n")
// );
