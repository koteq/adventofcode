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
    this.items = itemsStr.split(", ").map(BigInt);
    const [, left, operation, right] = operationStr.match(
      /new = (\S+) (\S) (\S+)/
    );
    this.operation = operation;
    this.left = left === 'old' ? null : BigInt(left);
    this.right = right === 'old' ? null: BigInt(right);
    const [, divisibleByStr] = testStr.match(/divisible by (\d+)/);
    this.testDivisibleBy = BigInt(divisibleByStr);
    this.targetIfTrue = Number(throwIfTrue);
    this.targetIfFalse = Number(throwIfFalse);
  }

  runOperation(worry) {
    const left = this.left ?? worry;
    const right = this.right ?? worry;
    return {
      "+": () => left + right,
      "*": () => left * right,
    }[this.operation]();
  }

  runTest(worry) {
    return worry != 0 && worry % this.testDivisibleBy == 0
      ? this.targetIfTrue
      : this.targetIfFalse;
  }

  throwItems(monkeys) {
    while (this.items.length) {
      this.inspected += 1;
      let worry = this.items.shift();
      worry = this.runOperation(worry);
      const limiter = monkeys.map(m => m.testDivisibleBy).reduce((acc, n) => acc * n, BigInt(1));
      worry = worry % limiter;
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

for (let round = 1; round <= 10000; round++) {
  monkeys.forEach((monkey) => monkey.throwItems(monkeys));
  if (
    [1, 20, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 9000, 10000].includes(
      round
    )
  ) {
    console.log(`\n== After round ${round} ==`);
    monkeys.forEach((m, i) =>
      console.log(`Monkey ${i} inspected items ${m.inspected} times.`)
    );
  }
}

const result = monkeys
  .map((m) => m.inspected)
  .sort((a, b) => b - a)
  .slice(0, 2)
  .reduce((acc, n) => acc * n, 1);
console.log(result);
