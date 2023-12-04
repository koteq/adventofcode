import fs from "fs";

const puzzle_input = "in";

class Card {
  constructor(data) {
    const [id_data, winning_data, yours_data] = data.split(/: | \| /);
    this.id = Number(id_data.match(/\d+/)[0]);
    this.winning = new Set(
      Array.from(winning_data.matchAll(/\d+/g)).map(([match]) => Number(match))
    );
    this.yours = Array.from(yours_data.matchAll(/\d+/g)).map(([match]) =>
      Number(match)
    );
  }

  get points() {
    let result = 0;
    for (let num of this.yours) {
      if (this.winning.has(num)) {
        result = result === 0 ? 1 : result * 2;
      }
    }
    return result;
  }
}

const lines = fs.readFileSync(puzzle_input).toString().trim().split("\n");
const cards = lines.map((line) => new Card(line));
const answer = cards.reduce((acc, card) => acc + card.points, 0);
console.log(answer);
