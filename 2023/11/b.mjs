import fs from "node:fs";

function main(inputFile = "in") {
  const data = fs.readFileSync(inputFile).toString().trim().split("\n");
  const universe = new Universe({ h: data.length, w: data[0].length });
  data.forEach((line, y) => {
    Array.from(line).forEach((chr, x) => {
      if (chr === "#") {
        universe.addGalaxy({ x, y });
      }
    });
  });
  universe.expand(1000000);

  let answer = 0;
  for (const [a, b] of universe.galaxyPairs()) {
    answer += Math.abs(a.x - b.x) + Math.abs(a.y - b.y);
  }
  console.log(answer);
}

class Universe {
  constructor({ w, h }) {
    this.bounds = { w, h };
    this.galaxies = [];
  }

  addGalaxy({ x, y }) {
    this.galaxies.push({ x, y });
  }

  expand(factor) {
    let xSet = new Set(this.galaxies.map(({ x }) => x));
    for (let x = 0; x < this.bounds.w; x++) {
      if (!xSet.has(x)) {
        this.galaxies.forEach((galaxy) => {
          if (galaxy.x > x) {
            galaxy.x += factor - 1;
          }
        });
        xSet = new Set(this.galaxies.map(({ x }) => x));
        this.bounds.w += factor - 1;
        x += factor - 1;
      }
    }
    let ySet = new Set(this.galaxies.map(({ y }) => y));
    for (let y = 0; y < this.bounds.h; y++) {
      if (!ySet.has(y)) {
        this.galaxies.forEach((galaxy) => {
          if (galaxy.y > y) {
            galaxy.y += factor - 1;
          }
        });
        ySet = new Set(this.galaxies.map(({ y }) => y));
        this.bounds.h += factor - 1;
        y += factor - 1;
      }
    }
  }

  *galaxyPairs() {
    for (let a = 0; a < this.galaxies.length; a++) {
      for (let b = a + 1; b < this.galaxies.length; b++) {
        yield [this.galaxies[a], this.galaxies[b]];
      }
    }
  }
}

main();
