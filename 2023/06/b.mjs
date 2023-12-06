import fs from "node:fs";

function main(input_file = "in") {
  const [timeData, distData] = fs
    .readFileSync(input_file)
    .toString()
    .trim()
    .split("\n");
  const time = Number(timeData.replace(/\s+/g, "").split(":").at(1));
  const dist = Number(distData.replace(/\s+/g, "").split(":").at(1));
  const race = new Race(time, dist);

  const answer = race.countWaysToWin();
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
