const [targetX, targetY] = [
  [137, 171],
  [-98, -73],
];

let total = 0;
for (let yVelInit = targetY[0]; yVelInit <= 100; yVelInit++) {
  for (let xVelInit = targetX[1]; xVelInit >= 0; xVelInit--) {
    const pos = [0, 0];
    let xVel = xVelInit;
    let yVel = yVelInit;
    while (pos[0] <= targetX[1] && pos[1] >= targetY[0]) {
      pos[0] += xVel;
      pos[1] += yVel;
      if (xVel > 0) {
        xVel -= 1;
      }
      yVel -= 1;
      if (
        targetX[0] <= pos[0] &&
        pos[0] <= targetX[1] &&
        targetY[0] <= pos[1] &&
        pos[1] <= targetY[1]
      ) {
        total += 1;
        console.log(total);
        break;
      }
    }
  }
}
