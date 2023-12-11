import fs from "node:fs";

function main(input_file = "in") {
  const data = fs.readFileSync(input_file).toString().trim().split("\n");
  const pipeline = new Pipeline(data);
  const animal = new Animal(pipeline);
  const steps = animal.runLoop(pipeline.start);
  const answer = steps / 2;
  console.log(answer);
}

const Direction = {
  East: "East",
  North: "North",
  West: "West",
  South: "South",
};

const Opposite = {
  [Direction.East]: Direction.West,
  [Direction.North]: Direction.South,
  [Direction.West]: Direction.East,
  [Direction.South]: Direction.North,
};

const Connections = new Map([
  ["|", new Set([Direction.North, Direction.South])],
  ["-", new Set([Direction.East, Direction.West])],
  ["L", new Set([Direction.North, Direction.East])],
  ["J", new Set([Direction.North, Direction.West])],
  ["7", new Set([Direction.South, Direction.West])],
  ["F", new Set([Direction.South, Direction.East])],
  [".", new Set([])],
  ["S", new Set([])],
]);

class Pipeline {
  constructor(data) {
    this.data = data;
  }

  at({ x, y }) {
    return this.data[y]?.[x] ?? ".";
  }

  next(pos, direction) {
    const offset = { x: 0, y: 0 };
    if (direction === Direction.East) {
      offset.x += 1;
    } else if (direction === Direction.North) {
      offset.y -= 1;
    } else if (direction === Direction.West) {
      offset.x -= 1;
    } else if (direction === Direction.South) {
      offset.y += 1;
    }
    const nextPos = { x: pos.x + offset.x, y: pos.y + offset.y };
    const nextDirection = Array.from(Connections.get(this.at(nextPos)).values())
      .filter((dir) => dir !== Opposite[direction])
      .at(0);

    return [nextPos, nextDirection];
  }

  get start() {
    if (!this._start) {
      for (let y = 0; y < this.data.length; y++) {
        const line = this.data[y];
        const x = line.indexOf("S");
        if (x !== -1) {
          this._start = { x, y };
          break;
        }
      }
    }
    return this._start;
  }

  get startDirections() {
    const { x, y } = this.start;
    const adjacent = [
      [Direction.East, { x: x + 1, y: y }],
      [Direction.North, { x: x, y: y - 1 }],
      [Direction.West, { x: x - 1, y: y }],
      [Direction.South, { x: x, y: y + 1 }],
    ];
    const result = adjacent
      .filter(([dir, pos]) => Connections.get(this.at(pos)).has(dir))
      .map(([dir]) => dir);
    return result;
  }
}

class Animal {
  constructor(pipeline) {
    this.pipeline = pipeline;
  }

  runLoop(start) {
    let steps = 0;
    let pos = start;
    let direction = this.pipeline.startDirections.at(0);
    do {
      steps += 1;
      [pos, direction] = this.pipeline.next(pos, direction);
    } while (pos.x !== start.x || pos.y !== start.y);
    return steps;
  }
}

main();
