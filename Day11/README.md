# Day 11

This problem is basically like the Game of Life, but with a complication that some squares can never be active and a little twist on the rules in part two.  In both parts, each cell acts independently, but with different rules for whether to turn on and off.  Clearly I can create a system that runs the game and takes in the rules as an argument.

The representation of the grid is probably the most interesting initial question.  It's clearly a two-dimensional array/matrix, but how to represent that?  My mind immediately goes to `Array`, but has a very awkward interface, in my opinion.  I could use a `Map`, but that seems like a waste when the structure is sparse.  Another one I can use is a `Set` which realizes that instead of storing simple bits for active or inactive, it's quite easy to just store the locations that are active and not waste time storing all the others.

I resolved to make this work with an `Array` since I haven't used them often and they seem useful.  In the past I have struggled with the `ST` monad, so I didn't want to add that complication.

Ultimately, a fairly simple solution with `Array` and otherwise naive algorithms resulted in a relatively fast solution, taking less than 350ms per part, which was enough to move on.

## Optimizations

There is one major thing that stands out for optimization: updating the data-structure.  I'm intrigued by the idea of representing the grid as a pair of sets. If it were just two states we could do one, but it is three so I need two.  I also plan to use `IntSet` because it is appreciably faster. This means mapping `R²⇒R`, but that is easy with fixed bounds.

Even a quick hack of this method is much faster.  The runtime goes down from above 300ms to under 20ms.

```