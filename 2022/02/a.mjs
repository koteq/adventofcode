import fs from "fs";

const shape = {
  rock: 1,
  paper: 2,
  scissors: 3,
};
const handScoreMap = {
  A: shape.rock,
  B: shape.paper,
  C: shape.scissors,
  X: shape.rock,
  Y: shape.paper,
  Z: shape.scissors,
};

function isWin(a, b) {
  return (
    (a === shape.rock && b === shape.scissors) ||
    (a === shape.scissors && b === shape.paper) ||
    (a === shape.paper && b === shape.rock)
  );
}

function isDraw(a, b) {
  return a === b;
}

const result = fs
  .readFileSync("in")
  .toString()
  .trim()
  .split("\n")
  .map((line) => line.split(" "))
  .map((hands) => hands.map((hand) => handScoreMap[hand]))
  .map(([opponent, me]) => {
    if (isWin(me, opponent)) return me + 6;
    if (isDraw(me, opponent)) return me + 3;
    return me;
  })
  .reduce((acc, score) => acc + score, 0);
console.log(result === 11873);
