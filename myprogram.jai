
var v: []int
// some variables
var var1 = (3+5)+5
var var2 = var1
var var3: int = 3
var var4: float = 3.01+5.0
var var5 = fun2(var4, var1)
var myvec2: vec2
var mycomp : compound
var var6 = fun3(myvec2, mycomp)
var var7: vec2 = {x = 3, y = 5}
var var8: vec3 = {x = 3, y = 5}
var var9: compound = {a = {x = 3, y = 5}}
var var10 = overfun(3, 3.0)
// var7 := vec2{x = 3, y = 5}

// some functions
fn fun1(a:int, b:float, c:int): int {/*this is a function body*/ }
fn fun2(a:float, b:int): int {/*this is a function body*/}
fn fun3(a:vec2, b:compound): vec2 { fun2(0.1, 3); }
fn fun4() {}
fn add(a: int, b: int): int {
  return a + b
}

// some foreign (C) functions
fn putchar(a:int) : int #foreign
fn rand() : int #foreign

// function overloading is OK
fn overfun(a: vec2) {};
fn overfun(b: vec3) {};
fn overfun(a: int, b: float) {};
var result = overfun(1, 1.0);

var some_identifier = 3;
// some_identifier := type {a:int}; // identifiers must be unique, this should error

// some structs
type compound {
  a: vec2
  b: vec3
}

type compound2 {
  a:int
  b:int
}

type vec3 {
  x: int
  y: int
  z: int
}

type vec2 {
  x: int
  y: int
}

// some nested structs
type B {
  v: vec2
  // a: A; // will cause error because A will contain itself
}

type A {
  b: B
}

type Weapon {
  damage: int
  durability: int
}

type Monster {
  gold: int
  health: int
  weapon: Weapon
}

fn newRichMonster(): Monster {
  var some_gold = 300
  var more_gold = (2 * 2300) + 300
  return {gold = some_gold + more_gold*2}
}

// should not compile
/*
fn should_not_compile() : int {
  // variable not declared!
  putchar(a)
  a := 3

  // recursive type
  /*
  type A {
    b: B
  }
  type B {
    a: A
  }
  */

  // no return value!
}
*/

fn main() {

  // this should be allowed for pointer type but not for scalar type
  // newRichMonster().gold = 3

  var arr: [2][2]int
  arr[0][0] = 1
  arr[1][0] = 1

  for a : arr {
    for b : a {
      putchar(54 + b)
    }
  }

}
