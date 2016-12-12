# High
 * Read statements in function bodies into AST
 * foreign functions (C bindings)
 * operator precedence
 * Type identifier for struct initializer


# Low
 * Multiple return values
 * check for duplicate function overloads
 * Safe pointer type conversion between type and a struct containing that type as first member (single inheritance in OOP). This is very handy when you have several type that should be able to be reached through a single pointer and evaluated at runtime. ASTs are a good example of this. You get back an expression, but it could be a function call, binary operation etc.