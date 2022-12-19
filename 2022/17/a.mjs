import fs from "fs";

// Parse input

const moves = fs
  .readFileSync("in")
  .toString()
  .trim()
  .split("")
  .map((chr) => (chr === ">" ? +1 : -1));

class Block extends Set {
  static parse(str) {
    const coords = str
      .split("\n")
      .flatMap((line, y) =>
        line.split("").map((chr, x) => (chr === "#" ? [x, y] : null))
      )
      .filter((coord) => coord !== null);
    return new Block(coords);
  }

  add(val) {
    return super.add(typeof val === "string" ? val : val.join(","));
  }

  has(val) {
    return super.has(typeof val === "string" ? val : val.join(","));
  }

  clone(offset) {
    return new Block(
      Array.from(this.values()).map((coords) => {
        const [x, y] = coords.split(",").map(Number);
        return [x + offset[0], y + offset[1]].join(",");
      })
    );
  }

  getMaxY() {
    return Math.max(
      ...Array.from(this.values()).map(
        (coords) => coords.split(",").map(Number)[1]
      )
    );
  }
}

const chamberWidth = 7;

class State extends Block {
  hasCollision(other) {
    for (const val of other) {
      const [x, y] = val.split(",").map(Number);
      if (this.has(val) || y < 0 || x < 0 || x >= chamberWidth) return true;
    }
    return false;
  }

  add(val) {
    for (const coords of val) {
      super.add(coords);
    }
  }
}

// Parse shapes

const shapes = `
####

.#.
###
.#.

###
..#
..#

#
#
#
#

##
##
`
  .trim()
  .split("\n\n")
  .map((str) => Block.parse(str));

function* loopedGenerator(arr) {
  while (true) {
    for (const item of arr) {
      yield item;
    }
  }
}

class Game {
  constructor(shapes, moves) {
    this.shapes = shapes;
    this.moves = moves;
    this.movingBlock = null;
    this.bottomEdge = -1;
    this.state = new State();
    this.spawnCount = 0;
  }

  play() {
    while (true) {
      if (!this.movingBlock) {
        this.movingBlock = this.spawnBlock();
        // this.print();
      }
      this.moveBlock();
      // this.print();
    }
  }

  spawnBlock() {
    const offset = [2, this.bottomEdge + 4];
    const block = this.shapes.next().value.clone(offset);
    this.spawnCount += 1;
    return block;
  }

  moveBlock() {
    const sideMove = this.moves.next().value;
    const moves = [
      { offset: [sideMove, 0], stopOnCollision: false }, // to a side
      { offset: [0, -1], stopOnCollision: true }, // downwards
    ];
    for (const { offset, stopOnCollision } of moves) {
      const moved = this.movingBlock.clone(offset);
      if (!this.state.hasCollision(moved)) {
        this.movingBlock = moved;
      } else if (stopOnCollision) {
        this.state.add(this.movingBlock);
        if (this.spawnCount === 2022) {
          console.log(this.movingBlock.getMaxY() + 1);
          process.exit(0);
        }
        this.bottomEdge = this.state.getMaxY();
        this.movingBlock = null;
        return;
      }
    }
  }

  print() {
    const maxY = (this.movingBlock ?? this.state).getMaxY()
    for (let y = maxY; y >= -1; y--) {
      const line = [];
      for (let x = -1; x <= chamberWidth; x++) {
        if (y === -1) {
          line.push(x === -1 || x === chamberWidth ? "+" : "-");
        } else if (x === -1 || x === chamberWidth) {
          line.push("|");
        } else {
          const coords = [x, y].join(",");
          if (this.state.has(coords)) {
            line.push("#");
          } else if (this.movingBlock?.has(coords)) {
            line.push("@");
          } else {
            line.push(".");
          }
        }
      }
      console.log(line.join(""));
    }
    console.log("\n");
  }
}

const game = new Game(loopedGenerator(shapes), loopedGenerator(moves));
game.play();
