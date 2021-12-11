import fs from "fs";

const data = fs
  .readFileSync("in")
  .toString()
  .trim()
  .split("\n")
  .map((line) => line.split("").map(Number));

class Grid {
  constructor(data) {
    this.data = data;
    this.maxY = data.length;
    this.maxX = data[0].length;
    this.flashCount = 0;
    // this.print("Before any steps");
  }

  simulate(steps) {
    for (let i = 0; i < steps; i++) {
      this.increaseEnergy();
      this.flash();
      // this.print(`After step ${i + 1}`);
    }
  }

  increaseEnergy() {
    for (let y = 0; y < this.maxY; y++) {
      for (let x = 0; x < this.maxX; x++) {
        this.data[y][x] += 1;
      }
    }
  }

  flash() {
    const energyLimit = 9;
    let flashed;
    do {
      flashed = false;
      for (let y = 0; y < this.maxY; y++) {
        for (let x = 0; x < this.maxX; x++) {
          if (this.data[y][x] > energyLimit) {
            this.data[y][x] = 0;
            this.flashCount += 1;
            flashed = true;

            [
              [y - 1, x],
              [y - 1, x + 1],
              [y, x + 1],
              [y + 1, x + 1],
              [y + 1, x],
              [y + 1, x - 1],
              [y, x - 1],
              [y - 1, x - 1],
            ].forEach(([y, x]) => {
              if (
                x >= 0 &&
                x < this.maxX &&
                y >= 0 &&
                y < this.maxY &&
                this.data[y][x] !== 0
              ) {
                this.data[y][x] += 1;
              }
            });
          }
        }
      }
    } while (flashed);
  }

  print(caption = "") {
    console.log(
      caption +
        ":\n" +
        this.data
          .map((row) => row.map((val) => (val > 9 ? "+" : val)).join(""))
          .join("\n") +
        "\n"
    );
  }
}

const grid = new Grid(data);
grid.simulate(100);
console.log(`Total flashes ${grid.flashCount}`);
