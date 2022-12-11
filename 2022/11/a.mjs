import fs from "fs";

class Monkey {
  constructor(str) {
    const [
      ,
      monkeyId,
      itemsStr,
      operationStr,
      testStr,
      throwIfTrue,
      throwIfFalse,
    ] = str.match(
      /^Monkey (\d+):\s+Starting items: ([^\n]+)\s+Operation: ([^\n]+)\s+Test: ([^\n]+)\s+If true: throw to monkey (\d+)\s+If false: throw to monkey (\d+)$/
    );
    this.inspected = 0;
    this.items = itemsStr.split(", ").map(Number);
    const [, left, operation, right] = operationStr.match(
      /new = (\S+) (\S) (\S+)/
    );
    this.operation = operation;
    this.left = Number(left);
    this.right = Number(right);
    const [, divisibleByStr] = testStr.match(/divisible by (\d+)/);
    this.testDivisibleBy = Number(divisibleByStr);
    this.targetIfTrue = Number(throwIfTrue);
    this.targetIfFalse = Number(throwIfFalse);
  }

  runOperation(worry) {
    const left = Number.isFinite(this.left) ? this.left : worry;
    const right = Number.isFinite(this.right) ? this.right : worry;
    return {
      "+": () => left + right,
      "*": () => left * right,
    }[this.operation]();
  }

  runTest(worry) {
    return worry !== 0 && worry % this.testDivisibleBy === 0
      ? this.targetIfTrue
      : this.targetIfFalse;
  }

  throwItems(monkeys) {
    while (this.items.length) {
      this.inspected += 1;
      let worry = this.items.shift();
      worry = this.runOperation(worry);
      worry = Math.trunc(worry / 3);
      const target = this.runTest(worry);
      monkeys[target].items.push(worry);
    }
  }
}

const monkeys = fs
  .readFileSync("in")
  .toString()
  .trim()
  .split("\n\n")
  .map((str) => new Monkey(str));

for (let round = 0; round < 20; round++) {
  monkeys.forEach((monkey) => monkey.throwItems(monkeys));
}

const result = monkeys
  .map((m) => m.inspected)
  .sort((a, b) => b - a)
  .slice(0, 2)
  .reduce((acc, n) => acc * n, 1);
console.log(result);
