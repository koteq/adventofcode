import fs from "fs";

const data = fs
  .readFileSync("in")
  .toString()
  .trim()
  .split("\n\n")
  .map((calorieGroups) =>
    calorieGroups
      .split("\n")
      .map(Number)
      .reduce((acc, cal) => acc + cal, 0)
  )
  .sort((a, b) => b - a);
console.log(data[0]);
