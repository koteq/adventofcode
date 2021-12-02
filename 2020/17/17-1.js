const { readFileSync } = require("fs");

let map = new Map();
const xyz = (x, y, z) => [x, y, z].join(",");

String(readFileSync("./17-1.in"))
  .split("\n")
  .forEach((line, y) => {
    Array.from(line).forEach((val, x) => {
      if (val === "#") {
        map.set(xyz(x, y, 0), true);
      }
    });
  });
// printMap(map, 0, 2, 0, 2, 0, 0);

function getActiveNeighbors(map, xo, yo, zo) {
  let result = 0;
  for (let z = -1; z <= 1; z++) {
    for (let y = -1; y <= 1; y++) {
      for (let x = -1; x <= 1; x++) {
        if (!(x === 0 && y === 0 && z === 0)) {
          const pos = xyz(xo + x, yo + y, zo + z);
          const hasActiveNeighnor = map.has(pos);
          if (hasActiveNeighnor) {
            result += 1;
          }
        }
      }
    }
  }
  return result;
}

function printMap(map, xlo, xhi, ylo, yhi, zlo, zhi, marker) {
  for (let z = zlo; z <= zhi; z++) {
    console.log(`\nz=${z}`);
    for (let y = ylo; y <= yhi; y++) {
      const line = [];
      for (let x = xlo; x <= xhi; x++) {
        if (marker && marker[0] === x && marker[1] === y && marker[2] === z) {
          line.push("?");
        } else {
          line.push(map.has(xyz(x, y, z)) ? "#" : ".");
        }
      }
      console.log(line.join(""));
    }
  }
  console.log("\n");
}

// Iterate life
const hi = 16;
const lo = -hi;
for (let i = 1; i <= 6; i++) {
  console.log(`Simulating cycle ${i}, with  ${map.size} active cubes`);
  const old = map;
  map = new Map();
  for (let z = lo; z < hi; z++) {
    for (let y = lo; y < hi; y++) {
      for (let x = lo; x < hi; x++) {
        const activeNeighbors = getActiveNeighbors(old, x, y, z);
        const isActive = old.has(xyz(x, y, z));
        if (isActive && (activeNeighbors === 2 || activeNeighbors === 3)) {
          map.set(xyz(x, y, z), true);
        }
        if (!isActive && activeNeighbors === 3) {
          map.set(xyz(x, y, z), true);
        }
      }
    }
  }
}

console.log(map.size);
