import fs from "fs";

const puzzle_input = "in";

class Game {
  constructor() {
    this.cards = new Map();
  }

  addCard(card) {
    this.cards.set(card.id, card);
  }

  play() {
    const min_id = Math.min(...Array.from(this.cards.keys()));
    const max_id = Math.max(...Array.from(this.cards.keys()));
    for (let id = min_id; id <= max_id; id++) {
      const card = this.cards.get(id);
      if (card.points) {
        for (let i = id + 1; i <= id + card.points; i++) {
          if (this.cards.has(i)) {
            this.cards.get(i).copies += card.copies;
          }
        }
      }
    }
  }

  get cardCount() {
    return Array.from(this.cards.values()).reduce(
      (acc, card) => acc + card.copies,
      0
    );
  }
}

class Card {
  constructor(data) {
    const [id_data, winning_data, yours_data] = data.split(/: | \| /);
    this.id = Number(id_data.match(/\d+/)[0]);
    const winning = new Set(
      Array.from(winning_data.matchAll(/\d+/g)).map(([match]) => Number(match))
    );
    const yours = Array.from(yours_data.matchAll(/\d+/g)).map(([match]) =>
      Number(match)
    );
    this.copies = 1;
    this.points = 0;
    for (let num of yours) {
      if (winning.has(num)) {
        this.points += 1;
      }
    }
  }
}

const lines = fs.readFileSync(puzzle_input).toString().trim().split("\n");
const game = new Game();
lines.map((line) => new Card(line)).forEach((card) => game.addCard(card));

game.play();
const answer = game.cardCount;
console.log(answer);
