import fs from "fs";

const input_file = "s1";

fs.readFileSync(input_file).toString().trim().split("\n");
