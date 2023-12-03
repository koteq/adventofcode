import fs from "fs";

function isDigit(char) {
  return char >= "0" && char <= "9";
}

function isSymbol(char) {
  return !isDigit(char) && char !== ".";
}

function getAdjacentNumbers(data, px, py) {
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
  const skip = new Set();
  const result = [];
  adjacent.forEach(([dx, dy]) => {
    const x = px + dx;
    const y = py + dy;
    if (0 <= y && y < data.length) {
      const line = data[y];
      if (0 <= x && x < line.length) {
        const char = line[x];
        if (isDigit(char)) {
          let buff = char;
          let rx = 1;
          while (0 <= x + rx && x + rx < line.length && isDigit(line[x + rx])) {
            buff = buff + line[x + rx];
            rx += 1;
          }
          let lx = 1;
          while (0 <= x - lx && x - lx < line.length && isDigit(line[x - lx])) {
            buff = line[x - lx] + buff;
            lx += 1;
          }
          if (!skip.has(`${x - lx + 1}:${y}`)) {
            result.push(Number(buff));
            skip.add(`${x - lx + 1}:${y}`);
          }
        }
      }
    }
  });
  return result;
}

class Engine {
  constructor(data) {
    this.data = data;
  }

  get gears() {
    return (function* iteratecGears(data) {
      for (let y = 0; y < data.length; y++) {
        const line = data[y];
        for (let x = 0; x < line.length; x++) {
          const char = line[x];
          if (char === "*") {
            const numbers = getAdjacentNumbers(data, x, y);
            if (numbers.length === 2) {
              yield numbers[0] * numbers[1];
            }
          }
        }
      }
    })(this.data);
  }
}

const data = fs.readFileSync("in").toString().trim().split("\n");
const engine = new Engine(data);
const answer = Array.from(engine.gears).reduce((acc, part) => acc + part, 0);
console.log(answer);
