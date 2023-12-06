import fs from "node:fs";

function main(input_file = "in") {
  const [timeData, distData] = fs
    .readFileSync(input_file)
    .toString()
    .trim()
    .split("\n");
  const times = timeData.split(/\s+/).slice(1).map(Number);
  const dists = distData.split(/\s+/).slice(1).map(Number);
  const races = times.map((time, i) => new Race(time, dists[i]));

  const answer = races
    .map((race) => race.countWaysToWin())
    .reduce((acc, cnt) => acc * cnt, 1);
  console.log(answer);
}

class Race {
  constructor(time, dist) {
    this.time = time;
    this.dist = dist;
  }

  countWaysToWin() {
    let wins = 0;
    for (let hold = 1; hold < this.time; hold++) {
      const speed = hold;
      const dist = speed * (this.time - hold);
      if (dist > this.dist) {
        wins += 1;
      }
    }
    return wins;
  }
}

main();
