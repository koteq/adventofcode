import fs from "node:fs";

function main(input_file = "in") {
  const lines = fs.readFileSync(input_file).toString().trim().split("\n");
  const answer = lines
    .map((line) => new History(line))
    .map((history) => history.extrapolate())
    .reduce((acc, val) => acc + val, 0);
  console.log(answer);
}

class History {
  constructor(data) {
    this.data = data.split(" ").map(Number);
  }

  extrapolate() {
    let values = this.data;
    let result = [values.at(0)];
    do {
      const diff = [];
      for (let i = values.length - 1; i > 0; i--) {
        diff.push(values[i] - values[i - 1]);
      }
      values = diff.reverse();
      result.push(values.at(0));
    } while (!values.every((val) => val === 0));
    let nextValue = 0;
    for (let i = result.length - 1; i >= 0; i--) {
      nextValue = result[i] - nextValue;
    }
    return nextValue;
  }
}

main();
