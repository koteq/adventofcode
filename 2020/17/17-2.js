const { readFileSync } = require("fs");

let map = new Map();
const xyzw = (x, y, z, w) => [x, y, z, w].join(",");

String(readFileSync("./17-1.in"))
  .split("\n")
  .forEach((line, y) => {
    Array.from(line).forEach((val, x) => {
      if (val === "#") {
        map.set(xyzw(x, y, 0, 0), true);
      }
    });
  });
// printMap(map, 0, 2, 0, 2, 0, 0);

function getActiveNeighbors(map, xo, yo, zo, wo) {
  let result = 0;
  for (let w = -1; w <= 1; w++) {
    for (let z = -1; z <= 1; z++) {
      for (let y = -1; y <= 1; y++) {
        for (let x = -1; x <= 1; x++) {
          if (!(x === 0 && y === 0 && z === 0 && w === 0)) {
            const pos = xyzw(xo + x, yo + y, zo + z, wo + w);
            const hasActiveNeighbor = map.has(pos);
            if (hasActiveNeighbor) {
              result += 1;
            }
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
const zwr = [-7, 7];
const xyr = [-7, 15];
for (let i = 1; i <= 6; i++) {
  console.log(`Simulating cycle ${i}, with  ${map.size} active cubes`);
  const old = map;
  map = new Map();
  for (let w = zwr[0]; w < zwr[1]; w++) {
    for (let z = zwr[0]; z < zwr[1]; z++) {
      for (let y = xyr[0]; y < xyr[1]; y++) {
        for (let x = xyr[0]; x < xyr[1]; x++) {
          const activeNeighbors = getActiveNeighbors(old, x, y, z, w);
          const isActive = old.has(xyzw(x, y, z, w));
          if (isActive && (activeNeighbors === 2 || activeNeighbors === 3)) {
            map.set(xyzw(x, y, z, w), true);
          }
          if (!isActive && activeNeighbors === 3) {
            map.set(xyzw(x, y, z, w), true);
          }
        }
      }
    }
  }
}

console.log(map.size);
