import fs from "fs";

let pos = 0;
let cwd = [];
const dirSizes = new Map();
const lines = fs.readFileSync("in").toString().trim().split("\n");
do {
  const [, command, argument] = lines[pos].match(/\$ (\S+)(?: (\S+))?/);
  switch (command) {
    case "cd": {
      if (argument === "..") {
        cwd.pop();
      } else {
        cwd.push(argument);
      }
      break;
    }
    case "ls": {
      let size = 0;
      do {
        pos += 1;
        size += Number((lines[pos].match(/(\d+) \S+$/) ?? [, "0"])[1]);
      } while (pos < lines.length - 1 && !lines[pos + 1].startsWith("$"));
      for (let i = cwd.length; i > 0; i--) {
        const path = cwd.slice(0, i).join("/");
        dirSizes.set(path, (dirSizes.get(path) ?? 0) + size);
      }
      break;
    }
  }
  pos += 1;
} while (pos < lines.length - 1);

let result = 0;
dirSizes.forEach((size) => {
  if (size <= 100000) {
    result += size;
  }
});
console.log(result);
