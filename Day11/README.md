# Day 11

This problem is basically like the Game of Life, but with a complication that some squares can never be active and a little twist on the rules in part two.  In both parts, each cell acts independently, but with different rules for whether to turn on and off.  Clearly I can create a system that runs the game and takes in the rules as an argument.

The representation of the grid is probably the most interesting initial question.  It's clearly a two-dimensional array/matrix, but how to represent that?  My mind immediately goes to `Array`, but has a very awkward interface, in my opinion.  I could use a `Map`, but that seems like a waste when the structure is sparse.  Another one I can use is a `Set` which realizes that instead of storing simple bits for active or inactive, it's quite easy to just store the locations that are active and not waste time storing all the others.

I resolved to make this work with an `Array` since I haven't used them often and they seem useful.  In the past I have struggled with the `ST` monad, so I didn't want to add that complication.

Ultimately, a fairly simple solution with `Array` and otherwise naive algorithms resulted in a relatively fast solution, taking less than 350ms per part, which was enough to move on.

## Optimizations

There is one major thing that stands out for optimization: updating the data-structure.  I'm intrigued by the idea of representing the grid as a pair of sets. If it were just two states we could do one, but it is three so I need two.  I also plan to use `IntSet` because it is appreciably faster. This means mapping `R²⇒R`, but that is easy with fixed bounds.

With a quick hack of this method it's not much faster, less than a 2x speed-up.  It still suffers from all the problems of having to do a lot of lookups and incremental updates of immutable data-structures. There may be some tricks I'm missing or some strictness I'm missing, but I'm not sure it's worth the time compared to...

Let's try using ST! We should be able to use unboxed mutable arrays to greatly reduce the work.  This turned out to be a huge pain, mostly due to the way that the ST monad works with its existential qualifier.  I spent at least half an hour just trying to figure out why `Grid . runSTUArray $ op` didn't work only to discover later that `Grid (runSTUArray op)` does work.  It seems to lose it's existential qualifier in the former case.

Ultimately, I got the ST version running and it's much faster, by a factor of about 10x.

### Final Benchmark

```
Parsing input...
benchmarking...
time                 264.2 μs   (261.3 μs .. 267.6 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 269.8 μs   (267.4 μs .. 272.1 μs)
std dev              7.975 μs   (6.753 μs .. 9.312 μs)
variance introduced by outliers: 24% (moderately inflated)

================================================================================
Running Part 1...
benchmarking...
time                 337.2 ms   (334.7 ms .. 339.9 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 341.5 ms   (339.4 ms .. 343.4 ms)
std dev              2.482 ms   (1.168 ms .. 3.108 ms)
variance introduced by outliers: 19% (moderately inflated)

Number of occupied seats after settling (Adjacent): 2470
================================================================================
Running Part 2...
benchmarking...
time                 430.0 ms   (398.4 ms .. 460.5 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 449.4 ms   (439.4 ms .. 459.1 ms)
std dev              11.40 ms   (9.801 ms .. 11.80 ms)
variance introduced by outliers: 19% (moderately inflated)

Number of occupied seats after settling (LOS): 2259
```
