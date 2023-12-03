import fs from "fs";

class Rules {
  static calcPower(game) {
    const max = {
      red: 0,
      green: 0,
      blue: 0,
    };
    game.turns.forEach((turn) => {
      turn.cubes.forEach(([count, color]) => {
        max[color] = Math.max(max[color], count);
      });
    });
    return max.red * max.green * max.blue;
  }
}

class Game {
  constructor(line) {
    this.id = Number(line.match(/Game (\d+): /)[1]);
    this.turns = line
      .replace(/Game (\d+): /, "")
      .split("; ")
      .map((data) => new Turn(data));
  }
}

class Turn {
  constructor(data) {
    this.cubes = Array.from(data.matchAll(/(\d+) (red|green|blue)/g)).map(
      ([_, count, color]) => [Number(count), color]
    );
  }
}

const lines = fs.readFileSync("in").toString().trim().split("\n");
const games = lines.map((line) => new Game(line));
const answer = games
  .map(Rules.calcPower)
  .reduce((acc, pow) => acc + pow, 0);
console.log(answer);
