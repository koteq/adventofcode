import process from "process";

function w(pos) {
  return ((pos - 1) % 10) + 1;
}

const map = new Map();
function dirac(p, pos, score) {
  const die = [
    1+1+1,
    1+1+2,
    1+1+3,
    1+2+1,
    1+2+2,
    1+2+3,
    1+3+1,
    1+3+2,
    1+3+3,
    2+1+1,
    2+1+2,
    2+1+3,
    2+2+1,
    2+2+2,
    2+2+3,
    2+3+1,
    2+3+2,
    2+3+3,
    3+1+1,
    3+1+2,
    3+1+3,
    3+2+1,
    3+2+2,
    3+2+3,
    3+3+1,
    3+3+2,
    3+3+3,
  ];
  const wins = [0, 0];
  const key = JSON.stringify([p, pos, score]);
  const dynamicWins = map.get(key);
  if (dynamicWins) {
    return dynamicWins;
  }
  for (const d of die) {
    const newPos = w(pos[p] + d);
    const newScore = score[p] + newPos;
    if (newScore >= 21) {
      wins[p] += 1;
    } else {
      const posCopy = [...pos];
      const scoreCopy = [...score];
      posCopy[p] = newPos;
      scoreCopy[p] = newScore;
      const subWins = dirac((p + 1) % 2, posCopy, scoreCopy);
      wins[0] += subWins[0];
      wins[1] += subWins[1];
    }
  }
  map.set(key, wins);
  return wins;
}

let rolls = 0;
const pos = [10, 4];
// const pos = [4, 8];
const score = [0, 0];
const wins = dirac(0, pos, score);
console.log(wins);
