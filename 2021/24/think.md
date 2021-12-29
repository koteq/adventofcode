inp w     inp w     inp w     inp w     inp w     inp w
mul x 0   mul x 0   mul x 0   mul x 0   mul x 0   mul x 0
add x z   add x z   add x z   add x z   add x z   add x z
mod x 26  mod x 26  mod x 26  mod x 26  mod x 26  mod x 26
div z 1   div z 1   div z 1   div z 1   div z 26  div z 26  <=>
add x 15  add x 15  add x 12  add x 13  add x -12 add x -14 <=>
eql x w   eql x w   eql x w   eql x w   eql x w   eql x w
eql x 0   eql x 0   eql x 0   eql x 0   eql x 0   eql x 0
mul y 0   mul y 0   mul y 0   mul y 0   mul y 0   mul y 0
add y 25  add y 25  add y 25  add y 25  add y 25  add y 25
mul y x   mul y x   mul y x   mul y x   mul y x   mul y x
add y 1   add y 1   add y 1   add y 1   add y 1   add y 1
mul z y   mul z y   mul z y   mul z y   mul z y   mul z y
mul y 0   mul y 0   mul y 0   mul y 0   mul y 0   mul y 0
add y w   add y w   add y w   add y w   add y w   add y w
add y 15  add y 10  add y 2   add y 16  add y 12  add y 13  <=>
mul y x   mul y x   mul y x   mul y x   mul y x   mul y x
add z y   add z y   add z y   add z y   add z y   add z y

```js
for (let i = 0; i < 14; i++) {
  w = input[i];   // 1
  x = z % 26      // 2,3,4
  if (i >= 7) {   // 5
    z /= 26
  }
  x += rand()           // 6
  x = (x === w) ? 0 : 1 // 7,8
  y = x * 25 + 1  // 9,10,11,12
  z *= y  // 13
  y = w + rand()  // 14,15,16
  y *= x
  z += y
}
```





































































inp w
mul x 0
add x z
mod x 26
div z 1
add x 10
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 11
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -9
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 5
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 1
add x 14
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 16
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 1
add x 13
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 6
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -14
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 15
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -11
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 3
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -2
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 12
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 26
add x -16
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 10
mul y x
add z y


















