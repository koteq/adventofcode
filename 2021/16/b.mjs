import fs from "fs";

const data = fs
  .readFileSync("in")
  .toString()
  .trim()
  .split("")
  .flatMap((char) => parseInt(char, 16).toString(2).padStart(4, "0").split(""));

class BITS {
  constructor(bits) {
    this.bits = bits;
    this.versionsSum = 0;
  }

  read(amount) {
    const buffer = [];
    for (let i = 0; i < amount; i++) {
      buffer.push(this.bits.shift());
    }
    return buffer.join("");
  }

  readPacket() {
    const { version, type } = this.readHeader();
    this.versionsSum += version;
    if (type === 4) {
      return this.readLiteralValue();
    } else {
      return this.readOperator(type);
    }
  }

  readHeader() {
    return {
      version: parseInt(this.read(3), 2),
      type: parseInt(this.read(3), 2),
    };
  }

  readLiteralValue() {
    const buffer = [];
    while (this.read(1) === "1") {
      buffer.push(this.read(4));
    }
    buffer.push(this.read(4));
    return parseInt(buffer.join(""), 2);
  }

  readOperator(operation) {
    const lengthType = this.read(1);
    const subPacketsLength = parseInt(this.read([15, 11][lengthType]), 2);
    const operands = [];
    if (lengthType === "0") {
      const initialLength = this.bits.length;
      const data = [];
      while (initialLength - this.bits.length < subPacketsLength) {
        operands.push(this.readPacket());
      }
    } else {
      for (let i = 0; i < subPacketsLength; i++) {
        operands.push(this.readPacket());
      }
    }
    const operationsMap = {
      0: (...args) => args.reduce((acc, val) => acc + val, 0),
      1: (...args) => args.reduce((acc, val) => acc * val, 1),
      2: (...args) => Math.min(...args),
      3: (...args) => Math.max(...args),
      5: (a, b) => a > b ? 1 : 0,
      6: (a, b) => a < b ? 1 : 0,
      7: (a, b) => a === b ? 1 : 0,
    };
    return operationsMap[operation](...operands);
  }
}

const bits = new BITS(data);

console.log(bits.readPacket());
