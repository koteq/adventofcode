import { readFileSync } from "fs";

const input = String(readFileSync("./13-1.in")).split("\n");
const buses = input[1]
  .split(",")
  .map(Number)
  .reduce((acc, id, idx) => {
    if (Number.isFinite(id)) {
      let offset = (id - idx) % id;
      if (offset < 0) {
        offset += id;
      }
      acc.push({ id: BigInt(id), offset: BigInt(offset) });
    }
    return acc;
  }, []);

const modulo = buses.reduce((acc, { id }) => acc * id, 1n);
const sum = buses.reduce((acc, bus) => {
  bus.modulo = modulo / bus.id;
  bus.inverseModulo = inverse(bus.modulo, bus.id);
  return acc + bus.offset * bus.modulo * bus.inverseModulo;
}, 0n);
console.log(sum % modulo);

function inverse(a, n) {
  let t = 0n;
  let r = n;
  let newt = 1n;
  let newr = a;
  while (newr !== 0n) {
    const quotient = r / newr;
    [t, newt] = [newt, t - quotient * newt];
    [r, newr] = [newr, r - quotient * newr];
  }
  if (r > 1n) {
    throw Error(`${a} is not invertible`);
  }
  if (t < 0n) {
    t = t + n;
  }
  return t;
}
