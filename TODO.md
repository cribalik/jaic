# High
 * struct member access
 * arrays
 * loops
 * pointers
 * operator precedence
 * Type identifier for struct initializer
 * Multiple return values

# Low
 * check for duplicate function overloads
 * Safe pointer type conversion between type and a struct containing that type as first member (single inheritance in OOP). This is very handy when you have several type that should be able to be reached through a single pointer and evaluated at runtime. ASTs are a good example of this. You get something that is an expression, but it could be a function call, binary operation, number literal etc.