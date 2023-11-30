import fs from "fs";

const blueprints = [
  {
    ore: { ore: 4 },
    clay: { ore: 4 },
    obsidian: { ore: 3, clay: 14 },
    geode: { ore: 2, obsidian: 7 },
  },
];

class Blueprint {
  static parse(...args) {
    return new Blueprint(...args);
  }

  constructor(str) {
    this.id = Number(str.match(/Blueprint (\d+)/)[1]);
    this.robots = Array.from(
      str.matchAll(/Each (\w+) robot costs (.*?)\./g)
    ).map(([, type, cost]) => Robot.parse(type, cost));
  }
}

class Robot {
  static parse(...args) {
    return new Robot(...args);
  }

  constructor(type, cost) {
    this.type = type;
    this.cost = cost.split(" and ").map((str) => {
      const [amount, type] = str.split(" ");
      return { [type]: Number(amount) };
    });
  }
}

const d = fs.readFileSync("s1").toString().trim().split("\n").map(Blueprint.parse);
console.log(d);
