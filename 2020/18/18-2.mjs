import { readFileSync } from "fs";

const isDigit = (char) => /[1-9]/.test(char);
const isNumber = (el) => typeof el === "number";
const isOperator = (char) => /[+*]/.test(char);
const isOpenBrace = (char) => char === "(";
const isCloseBrace = (char) => char === ")";

const operationsMap = {
  "+": (left, right) => left + right,
  "*": (left, right) => left * right,
};

const sum = "2 * 3 + (4 * 5)"
  .toString()
  .split("\n")
  .map((line) => {
    const result = prepare(line);
    while (result.length > 1) {
      calculate(result);
    }
    return result[0];
  })
  .reduce((acc, curr) => {
    console.log(curr);
    return acc + curr;
  }, 0);
console.log(sum);

function prepare(line) {
  const result = [];
  for (const char of line) {
    if (char !== " ") {
      if (isDigit(char)) {
        result.push(Number(char));
      } else {
        result.push(char);
      }
    }
  }
  return result;
}

function calculate(line, startAtPos = 0) {
  for (const allowedOperator of ["+", "*"]) {
    let leftOperand;
    let leftOperandPos = startAtPos;
    let rightOperand;
    let operator = null;
    let isOperationSkipped = false;

    for (let pos = startAtPos; pos < line.length; pos++) {
      const el = line[pos];
      if (isOperator(el)) {
        operator = el;
      } else if (isNumber(el)) {
        if (operator === allowedOperator) {
          rightOperand = el;
          leftOperand = operationsMap[operator](leftOperand, rightOperand);
          line.splice(leftOperandPos, pos - leftOperandPos + 1, leftOperand);
          console.log(line.join(""));
          pos = leftOperandPos;
        } else {
          if (operator != null) {
            isOperationSkipped = true;
          }
          leftOperand = el;
          leftOperandPos = pos;
        }
      } else if (isOpenBrace(el)) {
        const [subResult, newPos] = calculate(line, pos + 1);
        leftOperand = subResult;
        // leftOperand = operationsMap[operator](leftOperand, subResult);
        if (newPos) {
          pos = newPos;
        }
      }
      if (isCloseBrace(el)) {
        if (!isOperationSkipped) {
          line.splice(
            leftOperandPos - 1,
            pos - leftOperandPos + 2,
            leftOperand
          );
          console.log(line.join(""));
          return [leftOperand, pos - leftOperandPos + 2];
        }
        break;
      }
    }
  }
  // return;
  return [null, null];
}
