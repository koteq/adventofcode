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
      this.readLiteralValue();
    } else {
      this.readOperator();
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
    const value = parseInt(buffer.join(""), 2);
    console.log(`Literal ${value}`);
    return value;
  }

  readOperator() {
    const lengthType = this.read(1);
    const subPacketsLength = parseInt(this.read([15, 11][lengthType]), 2);
    if (lengthType === "0") {
      const initialLength = this.bits.length;
      const data = [];
      while (initialLength - this.bits.length < subPacketsLength) {
        this.readPacket();
      }
    } else {
      for (let i = 0; i < subPacketsLength; i++) {
        this.readPacket();
      }
    }
  }
}

const bits = new BITS(data);
bits.readPacket();
console.log(bits.versionsSum);
