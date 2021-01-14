# Day 14

It's another simulation one!  I like these.  It's nice to have an interesting set of inputs, then run it through a few steps and see what comes out.  The interesting part is always in the state transformations, ant this is particularly interesting with (effectively) unbounded memory.  At 36 bits, it's certainly a lot of memory to use.

Overall, the implementation ended up being pretty straightforward.  The hardest part is storing the memory mask and applying it.  I started with a pretty straight-forward implementation that applies the mask bit by bit since it was quick to implement and it worked.  The functions weren't even complicated enough to warrant using a state monad.

## Optimization

Clearly, something that can be improved is the application of the mask.  That could be more effective. I also noticed that the second part took about 200x times longer than the first (40ms vs 200μs)!  There may be something to work on there.

### Improving Masking

Since the mask has three states instead of two, it can't be represented as just a single number and have bitwise operations applied.  This is what moved me away from it to start.  Now, let's represent it a bit better.

In part one, we need to change some values to one, change some to zero and leave some unchanged.  Changing values to one is easy with or.  Changing values to zero can be done by inverting the bits and using "and".  The other values are just all bits that aren't set in either of the first two, `¬(ones ∨ zeroes)`.  This gives a nice speed up down to less than 60μs.

Another thing I noticed is that I was using (effectively) `Map Int` over `IntMap` in my zealousness to use a `newtype` wrapper and make memory locations distinct from values.  Switching to `IntMap` reduced runtime by about half, but at the loss of some type-safety in `Eval.hs`.
  
However, in part 2 there is the matter of floating values which makes calculation more complicated.  I tried doing some tricks in representing sets of memory values, but the intersections and difference got very complicated to the point I wasn't sure it was worth the time.

### Memory counting

One trick I did try was accumulating the memory in a list and then only computing the sum once at the end.  This was about as fast for part 2, but slowed down part 1, so the extra step is probably not worth the complication.

### Final Benchmark

```
Parsing input...
benchmarking...
time                 446.0 μs   (439.0 μs .. 452.9 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 440.7 μs   (437.8 μs .. 446.6 μs)
std dev              13.85 μs   (7.682 μs .. 24.88 μs)
variance introduced by outliers: 24% (moderately inflated)

================================================================================
Running Part 1...
benchmarking...
time                 141.4 μs   (141.0 μs .. 141.9 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 141.3 μs   (141.1 μs .. 141.6 μs)
std dev              877.0 ns   (740.7 ns .. 1.002 μs)

Sum of memory on termination (V1): 15514035145260
================================================================================
Running Part 2...
benchmarking...
time                 25.37 ms   (24.60 ms .. 26.30 ms)
                     0.995 R²   (0.988 R² .. 0.999 R²)
mean                 25.81 ms   (25.46 ms .. 26.38 ms)
std dev              971.4 μs   (627.0 μs .. 1.570 ms)

Sum of memory on termination (V2): 3926790061594
```
