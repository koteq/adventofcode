import fs from "node:fs";

function main(input_file = "in") {
  const lines = fs.readFileSync(input_file).toString().trim().split("\n");
  const hands = lines
    .map((line) => line.split(" "))
    .map(([hand, bid]) => new Hand(hand, Number(bid)))
    .sort(Hand.compare);
  const answer = hands.reduce(
    (acc, hand, idx) => acc + hand.bid * (idx + 1),
    0
  );
  console.log(answer);
}

class Hand {
  static compare(a, b) {
    if (a.strength.ofHand !== b.strength.ofHand) {
      return Math.sign(a.strength.ofHand - b.strength.ofHand);
    } else {
      return Math.sign(a.strength.ofCards - b.strength.ofCards);
    }
  }

  constructor(hand, bid) {
    this.hand = hand;
    this.bid = bid;
  }

  get strength() {
    if (!this._strength) {
      this._strength = CamelCards.getStrength(this.hand);
    }
    return this._strength;
  }
}

class CamelCards {
  static cards = Array.from("23456789TJQKA");

  static getStrength(hand) {
    const ofCards = Array.from(hand)
      .reverse()
      .reduce(
        (acc, card, idx) =>
          acc +
          CamelCards.cards.indexOf(card) * CamelCards.cards.length ** idx +
          CamelCards.cards.length ** (idx + 1),
        0
      );

    let ofHand = 0;
    const cardCounts = {};
    for (const card of hand) {
      cardCounts[card] = (cardCounts[card] ?? 0) + 1;
    }
    const counts = Object.values(cardCounts)
      .toSorted((a, b) => b - a)
      .filter((c) => c > 1);
    if (counts.at(0) === 5) {
      ofHand = 6;
    } else if (counts.at(0) === 4) {
      ofHand = 5;
    } else if (counts.at(0) === 3 && counts.at(1) === 2) {
      ofHand = 4;
    } else if (counts.at(0) === 3) {
      ofHand = 3;
    } else if (counts.at(0) === 2 && counts.at(1) === 2) {
      ofHand = 2;
    } else if (counts.at(0) === 2) {
      ofHand = 1;
    }

    return { ofHand, ofCards };
  }
}

main();
