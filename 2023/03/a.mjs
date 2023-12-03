import fs from "fs";

function isDigit(char) {
  return char >= "0" && char <= "9";
}

function isSymbol(char) {
  return !isDigit(char) && char !== ".";
}

function isAdjacentToSymbol(data, px, py) {
  const adjacent = [
    [-1, -1],
    [-1, 0],
    [-1, 1],
    [0, -1],
    [0, 1],
    [1, -1],
    [1, 0],
    [1, 1],
  ];
  return adjacent.some(([dx, dy]) => {
    const x = px + dx;
    const y = py + dy;
    if (0 <= y && y < data.length) {
      const line = data[y];
      if (0 <= x && x < line.length) {
        const char = line[x];
        return isSymbol(char);
      }
    }
    return false;
  });
}

class Engine {
  constructor(data) {
    this.data = data;
  }

  get parts() {
    return (function* iterateParts(data) {
      let buff = "";
      let isPartNumber = false;

      for (let y = 0; y < data.length; y++) {
        const line = data[y];
        for (let x = 0; x < line.length; x++) {
          const char = line[x];
          if (isDigit(char)) {
            buff += char;
            if (isAdjacentToSymbol(data, x, y)) {
              isPartNumber = true;
            }
          } else {
            if (buff !== "") {
              if (isPartNumber) {
                yield Number(buff);
              }
              buff = "";
              isPartNumber = false;
            }
          }
        }
        if (buff !== "") {
          if (isPartNumber) {
            yield Number(buff);
          }
          buff = "";
          isPartNumber = false;
        }
      }
    })(this.data);
  }
}

const data = fs.readFileSync("in").toString().trim().split("\n");
const engine = new Engine(data);
const answer = Array.from(engine.parts).reduce((acc, part) => acc + part, 0);
console.log(answer);
