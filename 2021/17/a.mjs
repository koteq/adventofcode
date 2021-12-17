// const target = [-10, -5];
const target = [-98, -73];

let currentYVel = -1;
let preCalcYVel = 0;
const positions = [0];
do {
  currentYVel += 1;
  while (positions[currentYVel] - positions[preCalcYVel] > target[1]) {
    preCalcYVel += 1;
    positions.push(positions[positions.length - 1] + preCalcYVel);
  }
  if (
    target[0] <= positions[currentYVel] - positions[preCalcYVel] &&
    positions[currentYVel] - positions[preCalcYVel] <= target[1]
  ) {
    console.log(positions[currentYVel], currentYVel);
  }
} while (true);
