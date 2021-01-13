# Day 14

It's another simulation one!  I like these.  It's nice to have an interesting set of inputs, then run it through a few steps and see what comes out.  The interesting part is always in the state transformations, ant this is particularly interesting with (effectively) unbounded memory.  At 36 bits, it's certainly a lot of memory to use.

Overall, the implementation ended up being pretty straightforward.  The hardest part is storing the memory mask and applying it.  I started with a pretty straight-forward implementation that applies the mask bit by bit since it was quick to implement and it worked.  The functions weren't even complicated enough to warrant using a state monad.

## Optimization

Clearly, something that can be improved is the application of the mask.  That could be more effective. I also noticed that the second part took about 200x times longer than the first (40ms vs 200μs)!  There may be something to work on there.

### Improving Masking

Since the mask has three states instead of two, it can't be represented as just a single number and have bitwise operations applied.  This is what moved me away from it to start.  Now, let's represent it a bit better.

In part one, we need to change some values to one, change some to zero and leave some unchanged.  Changing values to one is easy with or.  Changing values to zero can be done by inverting the bits and using "and".  The other values are just all bits that aren't set in either of the first two, `¬(ones ∨ zeroes)`.  This gives a nice speed up down to less than 60μs.

Another thing I noticed is that I was using (effectively) `Map Int` over `IntMap` in my zealousness to use a `newtype` wrapper and make memory locations distinct from values.  Switching to `IntMap` reduced runtime by about half, but at the loss of some type-safety in `Eval.hs`.
  
However, in part 2 there is the matter of floating values which makes calculation more complicated.  