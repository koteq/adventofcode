import fs from "fs";

const shape = {
  rock: 1,
  paper: 2,
  scissors: 3,
};
const need = {
  lose: 1,
  draw: 2,
  win: 3,
};
const handScoreMap = {
  A: shape.rock,
  B: shape.paper,
  C: shape.scissors,
  X: shape.rock,
  Y: shape.paper,
  Z: shape.scissors,
};

function getWinHand(h) {
  return (h % 3) + 1;
}

function getLoseHand(h) {
  return ((h + 1) % 3) + 1;
}

const result = fs
  .readFileSync("in")
  .toString()
  .trim()
  .split("\n")
  .map((line) => line.split(" "))
  .map((hands) => hands.map((hand) => handScoreMap[hand]))
  .map(([opponent, me]) => {
    if (me === need.win) return getWinHand(opponent) + 6;
    if (me === need.draw) return opponent + 3;
    return getLoseHand(opponent);
  })
  .reduce((acc, score) => acc + score, 0);
console.log(result);
