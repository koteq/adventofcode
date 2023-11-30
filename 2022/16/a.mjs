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
  .readFileSync("s1")
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
console.log(vertices);

let timeLeft = 30;
let startVertexId = "AA";
let totalPressure = 0;
const timeToReleaseVent = 1;
const edgeDistance = 1

while (timeLeft > 0) {
  // Find next vertex where it's preferable to open a vent
  const heap = new Heap((a, b) => a.distance - b.distance);
  for (const vertex of vertices) {
    vertex.visited = false;
    vertex.distance =
      vertex.id === startVertexId ? 0 : Number.POSITIVE_INFINITY;
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
      const newDistance =
        bestVertex.distance + edgeDistance;
      if (newDistance < neighborVertex.distance) {
        neighborVertex.distance = newDistance;
        // neighborVertex.previous = bestVertex; // do I need the path?
        heap.updateItem(neighborVertex);
      }
    }
  }

  function getPressure({ distance, flowRate }) {
    return Math.max(0, flowRate * (timeLeft - distance - timeToReleaseVent));
  }

  for (const v of vertices) {
    console.log(`${v.id} - ${getPressure(v)}`)
  }

  const goalVertex = vertices.reduce((goal, vertex) => {
    if (goal === null) {
      return vertex.flowRate > 0 ? vertex : null;
    }
    return getPressure(vertex) > getPressure(goal) ? vertex : goal;
  }, null);

  if (goalVertex === null) break;
  totalPressure += getPressure(goalVertex);
  timeLeft -= goalVertex.distance + timeToReleaseVent;
  startVertexId = goalVertex.id;
  goalVertex.flowRate = 0;
}

console.log(totalPressure);
