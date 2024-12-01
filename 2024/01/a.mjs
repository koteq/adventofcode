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
  let result = 0;
  for (let i = 0; i < locations.left.length; i++) {
    const dist = Math.abs(locations.left[i] - locations.right[i]);
    result += dist;
  }
  console.log(result);
}

main();
