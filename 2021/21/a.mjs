import process from "process";

let rolls = 0;
const pos = [10, 4];
const score = [0, 0];
for (let die = 1; true; die += 3) {
  const move = die * 3 + 3;
  const i = rolls % 2;
  pos[i] = ((pos[i] + move - 1) % 10) + 1;
  score[i] += pos[i];
  if (score[i] >= 1000) {
    const loserScore = score[(i + 1) % 2];
    console.log({
      rolls: (rolls + 1) * 3,
      winner: i + 1,
      winnerScore: score[i],
      loserScore,
      answer: loserScore * (rolls + 1) * 3,
    });
    process.exit(0);
  }
  rolls += 1;
}
