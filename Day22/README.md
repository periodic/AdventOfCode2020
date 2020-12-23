# Day 22

This was an interesting one because I changed my approach half-way through part two.  I structured the first problem as having a step function that is repeatedly applied to separate the game logic from the looping.  I took this approach into part two, but that quickly became a mess because the state became a stack and each step had to deal with adding or removing items from the stack.  Instead it proved much easier to use the native recursion to track state.

Of course, this didn't entirely work right away.  I had a small bug and I added logging through a `Writer` to figure out that I had my condition to recurse inverted.

The solution still rans a bit slow though, taking about 20 seconds to complete.

TODO: Improve performance.