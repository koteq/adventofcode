import fs from "node:fs";

function main(input_file = "in") {
  const data = fs
    .readFileSync(input_file)
    .toString()
    .trim()
    .split("\n")
    .map((line) => Array.from(line));
  const pipeline = new Pipeline(data);

  pipeline.removeNonLoopedPipes();
  pipeline.replaceStartWithPipe();
  pipeline.surroundWithPadding();
  pipeline.burnOuterTiles();

  const answer = pipeline.countInnerTiles();
  console.log(answer);
  // 63 is wrong
  // 700 is too high
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

function isTurn(p) {
  return p === "L" || p === "J" || p === "7" || p === "F";
}

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
    return nextPos;
  }

  isOutBound({ x, y }) {
    const isOutBound =
      x < 0 || y < 0 || y >= this.data.length || x >= this.data[0].length;
    return isOutBound;
  }

  nextPipe(pos, direction) {
    const nextPos = this.next(pos, direction);
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
    if (!this._startDirections) {
      const { x, y } = this.start;
      const adjacent = [
        [Direction.East, { x: x + 1, y: y }],
        [Direction.North, { x: x, y: y - 1 }],
        [Direction.West, { x: x - 1, y: y }],
        [Direction.South, { x: x, y: y + 1 }],
      ];
      this._startDirections = adjacent
        .filter(([dir, pos]) =>
          Connections.get(this.at(pos)).has(Opposite[dir])
        )
        .map(([dir]) => dir);
    }
    return this._startDirections;
  }

  removeNonLoopedPipes() {
    let pos = this.start;
    let direction = this.startDirections.at(0);
    const visited = new CoordsSet();
    do {
      visited.add(pos);
      [pos, direction] = this.nextPipe(pos, direction);
    } while (pos.x !== this.start.x || pos.y !== this.start.y);
    for (let y = 0; y < this.data.length; y++) {
      for (let x = 0; x < this.data[y].length; x++) {
        if (!visited.has({ x, y })) {
          this.data[y][x] = ".";
        }
      }
    }
    return;
  }

  replaceStartWithPipe() {
    const [char] = Array.from(Connections.entries()).find(
      ([pipe, directions]) =>
        this._startDirections.every((direction) => directions.has(direction))
    );
    this.data[this.start.y][this.start.x] = char;
  }

  surroundWithPadding() {
    this.data = [
      Array.from({ length: this.data[0].length }, () => "."),
      ...this.data,
      Array.from({ length: this.data[0].length }, () => "."),
    ];
    for (const line of this.data) {
      line.unshift(".");
      line.push(".");
    }
  }

  findPipeFromOutside() {
    let pos = { x: -1, y: 0 };
    let direction = Direction.East;
    const visited = new CoordsSet();
    const turn = {
      [Direction.East]: Direction.South,
      [Direction.South]: Direction.West,
      [Direction.West]: Direction.North,
      [Direction.North]: Direction.East,
    };
    do {
      let nextPos = this.next(pos, direction);
      if (this.isOutBound(nextPos) || visited.has(nextPos)) {
        direction = turn[direction];
        nextPos = this.next(pos, direction);
      }
      pos = nextPos;
      visited.add(pos);
    } while (this.at(pos) === ".");
    return { pipePos: pos, outsideDirection: Opposite[direction] };
  }

  burnOuterTiles() {
    const visited = new CoordsSet();
    const stack = this.getOuterTilesAlongPipeline().values();
    while (stack.length) {
      const pos = stack.pop();
      if (!visited.has(pos)) {
        visited.add(pos);
        [
          Direction.East,
          Direction.South,
          Direction.West,
          Direction.North,
        ].forEach((dir) => {
          const nextPos = this.next(pos, dir);
          if (
            !this.isOutBound(nextPos) &&
            this.at(nextPos) === "." &&
            !visited.has(nextPos)
          ) {
            stack.push(nextPos);
          }
        });
      }
    }
    for (const { x, y } of visited.values()) {
      if (!this.isOutBound({ x, y })) {
        this.data[y][x] = " ";
      }
    }
  }

  countInnerTiles() {
    let count = 0;
    for (let y = 0; y < this.data.length; y++) {
      for (let x = 0; x < this.data[y].length; x++) {
        if (this.data[y][x] === ".") {
          count += 1;
        }
      }
    }
    return count;
  }

  getOuterTilesAlongPipeline() {
    const outside = new CoordsSet();
    const addOutside = (fromPos, outsideDirection) => {
      const pos = this.next(fromPos, outsideDirection);
      if (this.at(pos) === ".") {
        outside.add(pos);
      }
    };
    let { pipePos: start, outsideDirection } = this.findPipeFromOutside();
    let pos = start;
    let direction = Array.from(Connections.get(this.at(start))).at(0);
    do {
      const prevDirection = direction;
      [pos, direction] = this.nextPipe(pos, direction);
      if (direction !== prevDirection) {
        if (direction === outsideDirection) {
          // inner turn
          outsideDirection = Opposite[prevDirection];
        } else {
          // outer turn
          addOutside(pos, outsideDirection);
          outsideDirection = prevDirection;
          addOutside(pos, outsideDirection);
        }
      } else {
        // straight
        addOutside(pos, outsideDirection);
      }
    } while (pos.x !== start.x || pos.y !== start.y);
    return outside;
  }
}

class CoordsSet {
  constructor() {
    this._set = new Set();
  }

  add({ x, y }) {
    return this._set.add(`${x}:${y}`);
  }

  has({ x, y }) {
    return this._set.has(`${x}:${y}`);
  }

  values() {
    return Array.from(this._set.values()).map((line) => {
      const [x, y] = line.split(":").map(Number);
      return { x, y };
    });
  }
}

main();
