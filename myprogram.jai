var1 := (3.0); // implicit type
var2 := var1; // implicit type
var3 : int = 3;
var4 : float = 3.01;
var5 := fun2(1.0, var1)

// declare a variable of type vec2, even though vec2 isn't defined yet!
// TODO: struct initializers
// var5: vec2;


// some functions
fun1 := (a:int, b:float, c:int) -> int {/*this is a function body*/ };
fun2 := (a:int, b:int) -> int {/*this is a function body*/};
fun3 := () {};


// some types
/*
vec3 := {
  x: int;
  y: int;
  z: int;
};

vec2 := {
  x: int;
  y: int;
};

vec1 := {
  x: int;
};
*/

// a := vec2{x = 1, y = 3};
// b := vec2{x = 1, y = 3};
// c: vec2 = {x = 1, y = 3};
// d: vec2 = {x = 1, y = 3};

distance := (a: vec2, b: vec2) -> float {
  return sqrt((a.x-b.x)*(a.x-b.x) + (a.y-b.y)*(a.y-b.y));
}
// calculate distance between two points
// result := distance()
// myval := distance(vec2{x=1, y=2}, vec2{x=2, y=3});