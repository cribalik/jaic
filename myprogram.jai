// some variables
var1 := (3)
var2 := var1
var3 : int = 3
var4 : float = 3.01
var5 := fun2(var4, var1)
myvec2 : vec2
mycomp : compound
var6 := fun3(myvec2, mycomp)
var7 : vec2 = {x = 3, y = 5}
var8 : vec3 = {x = 3, y = 5}
var9 : compound = {a = {x = 3, y = 5}}
var10 := overfun({}, {})
// var7 := vec2{x = 3, y = 5}

// some functions
fun1 := (a:int, b:float, c:int) -> int {/*this is a function body*/ }
fun2 := (a:float, b:int) -> int {/*this is a function body*/}
fun3 := (a:vec2, b:compound) -> vec2 {/*this is a function body*/}
fun4 := () {}

// function overloading is OK
overfun := (a: vec2) {};
overfun := (b: vec3) {};
overfun := (a: int, b: float) {};
result := overfun(1, 1.0);

some_identifier := 3;
// some_identifier := {a:int;}; // identifiers must be unique, this will error

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


// a: vec2 = {x = 1, y = 3};
// b := vec2{x = 1, y = 3};

// calculate distance between two points
// dist := distance({x=1, y=2}, {x=2, y=3});