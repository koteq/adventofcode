import fs from "node:fs";

function main(input_file = "s1") {
  fs.readFileSync(input_file).toString().trim().split("\n");
}

main();
