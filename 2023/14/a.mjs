import fs from "node:fs";

function main(inputFile = "s1") {
  fs.readFileSync(inputFile).toString().trim();
}

class Platform {
  constructor(data) {
    const width = data.indexOf("\n");
    const height = Math.ceil(data.length / (width + 1));
    this.cols = [];
    for (let x = 0; x < width; x++) {
      const col = [];
      for (let y = 0; y < height; y++) {
        const char = data[x + y * (width + 1)];
        col.push(char);
        // if (char === "O") {
        //   col.rnd += 1 << y;
        // } else if (char === "#") {
        //   col.sqr += 1 << y;
        // }
      }
      this.cols.push(col);
    }
  }

  tilt() {
    for(const col of this.cols) {
      
    }
  }
}

main();
