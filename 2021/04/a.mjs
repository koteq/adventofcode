import { readFileSync } from "fs";

class Game {
  constructor(boards) {
    this.boards = boards;
  }

  play(numbers) {
    for (const number of numbers) {
      this.boards.forEach((board) => board.cross(number));

      const winner = this.getWinner();
      if (winner) {
        const score = this.getScore(winner, number);
        return { winner, score };
      }
    }
  }

  getWinner() {
    for (const board of this.boards) {
      if (board.hasBingo) {
        return board;
      }
    }
  }

  getScore(/** @type {Set<number>} */ board, lastNumber) {
    const sum = Array.from(board.numbers).reduce((carry, number) => carry + number, 0);
    return sum * lastNumber;
  }
}

class Board {
  constructor(data) {
    /** @type {number[][]} */
    this.data = data.split("\n").map((s) => s.trim().split(/\s+/).map(Number));
    this.numbers = new Set(this.data.flat());
    const rows = this.data.map((d) => new Set(d));
    const cols = [];
    for (let i = 0; i < this.data[0].length; i++) {
      cols.push(new Set(this.data.map((d) => d[i])));
    }
    /** @type {Set<number>[]} */
    this.lines = [...rows, ...cols];
  }

  cross(number) {
    this.lines.forEach((line) => line.delete(number));
    this.numbers.delete(number);
  }

  get hasBingo() {
    return this.lines.some((line) => line.size === 0);
  }
}

function main() {
  const data = readFileSync("in").toString().trim().split("\n\n");
  const numbers = data[0].split(",").map(Number);
  const boards = data.slice(1).map((d) => new Board(d));

  const game = new Game(boards);
  const winner = game.play(numbers);
  console.log(winner.score);
}

main();
