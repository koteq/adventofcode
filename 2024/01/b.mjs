import fs from "node:fs";

const compareNumbers = (a, b) => a - b;

function main(inputFile = "in") {
  const locations = { left: [], right: [] };
  fs.readFileSync(inputFile)
    .toString()
    .trim()
    .split("\n")
    .forEach((line) => {
      const [l, r] = line.split("   ").map(Number);
      locations.left.push(l);
      locations.right.push(r);
    });
  locations.left.sort(compareNumbers);
  locations.right.sort(compareNumbers);
  const rightMap = {};
  for (const n of locations.right) {
    rightMap[n] = (rightMap[n] ?? 0) + 1;
  }
  let result = 0;
  for (const n of locations.left) {
    result += n * (rightMap[n] ?? 0);
  }
  console.log(result);
}

main();
