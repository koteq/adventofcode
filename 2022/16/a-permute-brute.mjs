import fs from "fs";
import Heap from "./heap.js";

class Vertex {
  constructor(str) {
    const [, id, flowRate, neighbors] = str.match(
      /^Valve (\S+) has flow rate=(\d+); tunnels? leads? to valves? (.*?)$/
    );
    this.id = id;
    this.flowRate = Number(flowRate);
    this.neighbors = neighbors.split(", ");
  }
}

// Parse input
const vertices = fs
  .readFileSync("in")
  .toString()
  .trim()
  .split("\n")
  .map((line) => new Vertex(line));

// Replace neighbor ids with vertex references
for (const vertex of vertices) {
  vertex.neighbors = vertex.neighbors.map((neighborId) =>
    vertices.find(({ id }) => id === neighborId)
  );
}
// console.log(vertices);

const timeToReleaseVent = 1;
const edgeDistance = 1;

function permute(inputArr) {
  let result = [];

  const permute = (arr, m = []) => {
    if (arr.length === 0) {
      result.push(m);
    } else {
      for (let i = 0; i < arr.length; i++) {
        let curr = arr.slice();
        let next = curr.splice(i, 1);
        permute(curr.slice(), m.concat(next));
      }
    }
  };

  permute(inputArr);

  return result;
}

function getDistance(start, goal) {
  // Find next vertex where it's preferable to open a vent
  const heap = new Heap((a, b) => a.distance - b.distance);
  for (const vertex of vertices) {
    vertex.visited = false;
    vertex.distance = vertex.id === start ? 0 : Number.POSITIVE_INFINITY;
    heap.push(vertex);
  }

  // Dijkstra
  while (!heap.empty()) {
    const bestVertex = heap.pop();
    bestVertex.visited = true;
    const unvisitedNeighbors = bestVertex.neighbors.filter(
      ({ visited }) => visited === false
    );
    for (const neighborVertex of unvisitedNeighbors) {
      const newDistance = bestVertex.distance + edgeDistance;
      if (newDistance < neighborVertex.distance) {
        neighborVertex.distance = newDistance;
        heap.updateItem(neighborVertex);
      }
    }
  }

  return vertices.find(({ id }) => id === goal).distance;
}

const vertexIdsToPermute = vertices
  .filter(({ id, flowRate }) => id !== "AA" && flowRate > 0)
  .map(({ id }) => id);
console.log(`Begin permutations for ${vertexIdsToPermute.length}`);
const permutations = permute(vertexIdsToPermute);
console.log(`Finish permutations ${permutations.length}`);

const results = [];
for (const steps of permutations) {
  let start = "AA";
  let timeLeft = 30;
  let totalPressure = 0;
  for (const goal of steps) {
    const goalVertex = vertices.find(({ id }) => id === goal);
    const distance = getDistance(start, goal);
    const pressure = Math.max(
      0,
      goalVertex.flowRate * (timeLeft - distance - timeToReleaseVent)
    );
    totalPressure += pressure;
    start = goal;
    timeLeft -= distance + timeToReleaseVent;
  }
  results.push(totalPressure);
}

const best = Math.max(...results);
console.log(best);
