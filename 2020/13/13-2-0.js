const numbers = [2, 3, 5, 7];
const offsets = [-5, -8, 0, 4];
const lines = ["", "", "", ""];
let i = 0;
while (true) {
  i += 1;
  numbers.forEach((n, idx) => {
    lines[idx] += (i + offsets[idx]) % n === 0 ? "|" : " ";
  });
  if (numbers.every((n, idx) => (i + offsets[idx]) % n === 0)) {
    console.log("Met at", i);
    lines.forEach((line) => console.log(line));
    process.exit(0);
  }
}
