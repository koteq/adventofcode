import { readFileSync } from "fs";

const isDigit = (char) => /[1-9]/.test(char);
const isOperator = (char) => /[+*]/.test(char);
const isOpenBrace = (char) => char === "(";
const isCloseBrace = (char) => char === ")";

const operationsMap = {
  "+": (left, right) => left + right,
  "*": (left, right) => left * right,
};

const sum = readFileSync("./18-1.in")
  .toString()
  .split("\n")
  .map((line) => calculate(line))
  .reduce((acc, curr) => acc + curr, 0);
console.log(sum);

function calculate(line, startAtPos = 0) {
  let operator = "+";
  let leftOperand = 0;
  let rightOperand = 0;
  for (let pos = startAtPos; pos < line.length; pos++) {
    const char = line[pos];
    if (isOperator(char)) {
      operator = char;
    } else if (isDigit(char)) {
      rightOperand = Number(char);
      leftOperand = operationsMap[operator](leftOperand, rightOperand);
    } else if (isOpenBrace(char)) {
      const [subResult, newPos] = calculate(line, pos + 1);
      leftOperand = operationsMap[operator](leftOperand, subResult);
      pos = newPos;
    }
    if (isCloseBrace(char)) {
      return [leftOperand, pos];
    }
  }
  return leftOperand;
}
