import fs from "node:fs";

function main(inputFile = "s1") {
  fs.readFileSync(inputFile).toString().trim().split("\n");
}

main();
