# High
 * global variables
 * return void
 * don't access variables not yet declared
 * check for structs containing themselves
 * loops with arrays
 * pointers
 * differentiate between expressions that yield temporary, in-place values (struct initializers, binary operators, calls), and expressions that point to something on stack or in memory (variables and member variables of variables)
 * operator precedence
 * Type identifier for struct initializer
 * Multiple return values
 * (strongly and loosly typed) enums
 * (auto-breaking! :D) case (switch) statement
 * arrays of <enum> size
 * default function parameters
 * UFC
 * parse negative values
 * function pointers
 * any type

# Low
 * send in arguments to main
 * check if all code paths have return statements
 * check for duplicate function overloads
 * check for unused variables
 * `getPositionOfPlayer().x = 3` should only work for pointers?
 * check for conflicting declarations in same scope
 * No inheritence - always use composition because it scales better. As far as I can see the bonuses of inheritence (disregarding runtime polymorphism, which I'm not sure we will have) are:
    1. if player derives from position, you can just write `player.x`, instead of `player.position.x`. nice!
    2. you get automatic conversion to the parent type: `offsetPosition(player, x=1)`
    3. if you know a pointer to a type is really a pointer to the subtype, you can safely cast it to the subtype: `player := cast(pos, Player)`
    4. your subtype has the same member functions as the parent: `player.offset(x=1)`

  Basically, all of these features come down to implicit casting to parent type in different situations (even in point 4, since we don't have members functions but use UFC instead). If you want these features, why not have a member of this type, and annotate it to allow implicit casting with a keyword like C++ `using`?
  This makes it a lot easier to see what the type is made of, especially if we have multiple members of the same type
  ```
  Player := type {
    using Position pos // the main position of player
    Position pet_position // the position of its pet

    using Color color // main color of player
    Color hat_color // color of players hat, regardless of players collor
  }

  player: Player
  player.pet_position.offset(y=2)
  player.offset(x=1)
  player.hat_color.lighten(10) // make the hat color a bit lighter
  player.darken(10) // make player a bit darker
  ```
