import fs from "fs";

class Rules {
  static max = {
    red: 12,
    green: 13,
    blue: 14,
  };

  static isPossible(game) {
    return game.turns.every((turn) => {
      const counter = {
        red: 0,
        green: 0,
        blue: 0,
      };

      turn.cubes.forEach(([count, color]) => {
        counter[color] += count;
      });

      return (
        counter.red <= Rules.max.red &&
        counter.green <= Rules.max.green &&
        counter.blue <= Rules.max.blue
      );
    });
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
  .filter(Rules.isPossible)
  .reduce((acc, game) => acc + game.id, 0);
console.log(answer);
