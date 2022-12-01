import fs from "fs";

const scanners = new Map();
fs.readFileSync("s0")
  .toString()
  .trim()
  .split("\n\n")
  .forEach((scannerData) => {
    const [firstLine, ...beaconsData] = scannerData.split("\n");
    const scanner = firstLine.match(/\d+/)[0];
    const beacons = beaconsData.map((pointData) =>
      pointData.split(",").map(Number)
    );
    scanners.set(scanner, beacons);
  });

function sinCos(a) {
  return [
    [0, 1],
    [1, 0],
    [0, -1],
    [-1, 0],
  ][a];
}

function rotateX([x, y, z], a) {
  const [sin, cos] = sinCos(a);
  return [
    x * 1 + y * 0 + z * 0,
    x * 0 + y * cos + z * -sin,
    x * 0 + y * sin + z * cos,
  ];
}

function rotateY([x, y, z], a) {
  const [sin, cos] = sinCos(a);
  return [
    x * cos + y * 0 + z * sin,
    x * 0 + y * 1 + z * 0,
    x * -sin + y * 0 + z * cos,
  ];
}

function rotateZ([x, y, z], a) {
  const [sin, cos] = sinCos(a);
  return [
    x * cos + y * -sin + z * 0,
    x * sin + y * cos + z * 0,
    x * 0 + y * 0 + z * 1,
  ];
}

function orientations(beacons) {
  const rMap = {
    X: rotateX,
    Y: rotateY,
    Z: rotateZ,
  };
  const rotations = [
    "X",
    "Y",
    "Z",
    "XX",
    "XY",
    "XZ",
    "YX",
    "YY",
    "ZY",
    "ZZ",
    "XXX",
    "XXY",
    "XXZ",
    "XYX",
    "XYY",
    "XZZ",
    "YXX",
    "YYY",
    "ZZZ",
    "XXXY",
    "XXYX",
    "XYXX",
    "XYYY",
  ];
  return rotations.map((rotation) => {
    return beacons.map((coords) => {
      let result = [...coords];
      rotation.split("").forEach((r) => {
        result = rMap[r](result, 1);
      });
    });
  });
}

function intersections(a, b) {
  const oA = orientations(a);
  const oB = orientations(b);
}

const [a, b] = scanners;

intersections(a, b);
