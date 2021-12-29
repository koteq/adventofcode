import process from "process";

function validate(model) {
  let w = 0;
  let x = 0;
  let y = 0;
  let z = 0;
  const r1 = [15, 15, 12, 13, -12, 10, -9, 14, 13, -14, -11, -2, -16, -14];
  const r2 = [15, 10, 2, 16, 12, 11, 5, 16, 6, 15, 3, 12, 10, 13];
  for (let i = 0; i < 14; i++) {
    w = model[i]; // 1
    x = z % 26; // 2,3,4
    if ([4, 6, 9, 10, 11, 12, 13].includes(i)) {
      z = Math.floor(z / 26); // 5
    }
    x += r1[i]; // 6
    if ([4, 6, 9, 10, 11, 12, 13].includes(i)) {
      if (x < 1 || x > 9) {
        return;
      }
      model[i] = x;
      w = model[i];
    }
    x = x === w ? 0 : 1; // 7,8
    y = x * 25 + 1; // 9,10,11,12
    z *= y; // 13
    y = (w + r2[i]) * x; // 14,15,16
    z += y;
  }
  if (z === 0) {
    console.log({ w, x, y, z }, model.join(""));
    process.exit(0);
  }
}

// for (let m = 4782968; m > 0; m--) {
for (let m = 0; m < 4782968; m++) {
  const model = m
    .toString(9)
    .padStart(7, 0)
    .split("")
    .map(Number)
    .map((n) => n + 1);
  model.splice(4, 0, undefined);
  model.splice(6, 0, undefined);
  model.splice(9, 0, undefined, undefined, undefined, undefined, undefined);
  validate(model);
}
