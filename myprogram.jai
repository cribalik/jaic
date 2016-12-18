
// some variables
var1 := (3+5)+5
var2 := var1
var3 : int = 3
var4 : float = 3.01+5.0
var5 := fun2(var4, var1)
myvec2: vec2
mycomp : compound
var6 := fun3(myvec2, mycomp)
var7 : vec2 = {x = 3, y = 5}
var8 : vec3 = {x = 3, y = 5}
var9 : compound = {a = {x = 3, y = 5}}
var10 := overfun(3, 3.0)
// var7 := vec2{x = 3, y = 5}

// some functions
fun1 := (a:int, b:float, c:int) -> int {/*this is a function body*/ }
fun2 := (a:float, b:int) -> int {/*this is a function body*/}
fun3 := (a:vec2, b:compound) -> vec2 { fun2(0.1, 3); }
fun4 := () {}

// some foreign (C) functions
putchar := (a:int) -> int #foreign
rand := () -> int #foreign

// function overloading is OK
overfun := (a: vec2) {};
overfun := (b: vec3) {};
overfun := (a: int, b: float) {};
result := overfun(1, 1.0);

some_identifier := 3;
// some_identifier := type {a:int}; // identifiers must be unique, this should error

// some structs
compound := type {
  a: vec2
  b: vec3
}

compound2 := type {
  a:int
  b:int
}

vec3 := type {
  x: int
  y: int
  z: int
}

vec2 := type {
  x: int
  y: int
}

// some nested structs
B := type {
  v: vec2
  // a: A; // will cause error because A will contain itself
}

A := type {
  b: B
}

Weapon := type {
  damage: int
  durability: int
}

Monster := type {
  gold: int
  health: int
  weapon: Weapon
}

print := (a:int) -> int {
  putchar(a)
}

newRichMonster := () -> Monster {
  some_gold := 300
  more_gold := 300 + 1700
  return {gold = some_gold + more_gold}
}

main := () {

  monster: Monster = {
    gold = rand(),
    health = 20,
    weapon = {damage = 3, durability = 10}
  }

  monster.gold = 20
  monster = {gold = 20}

  // this should be allowed for pointer type but not for scalar type
  // newMonster().gold = 20 

  /*
  loop v,i : array {
    print(i)
  }
  */

  for x,i : 97->110 {
    print(x)
    print(i+97)
    print(97+rand())
  }

}
