# Day 10

These problems are basically just a few list traversal algorithms.

The first one isn't too hard if you just remember to add the start and end elements. Theres a nice trick to getting all the diffs by doing

```haskell
differences :: [Int] -> [Int]
differences joltages =
    zipWith (-) (tail joltages) joltages
```

From there you can just just fold it into some counts, build a map out of it, or even just filter it twice.  Either way, it's pretty fast.

The second part, the main thing to realize is that you have to memoize as you go and you can't just brute-force it repeatedly.  My first version of this built up a map with a few recursive functions.  This took about 5µs.  I switched over to a terser lazy map, similar to Day 7, but that increased the runtime to 9µs.  However, it's much better looking, so I'm leaving it at that.

## Optimizations

There are a few small optimizations I can make.  For example, in the first part I was filtering twice.  It's possible to do that in one pass, which halves the time required, getting it under a microsecond.

There's a lot to take out if we look a little closer at the structure of part 2.  We can see that if we move up the list of joltages we only ever look back by at most three previous ones.  Also, we don't actually care what those joltages actually are, just where they are relative to the current value.  That means we only have to keep track of four values after each iteration, `j`, `c(j)`, `c(j-1)` and `c(j-2)`.  These can just be stored in a tuple and then we can use a quick `foldl` to calculate the result.  This drops the runtime by a factor of 20 compared to the solution using a lazy `Map` and a factor of 10 compared to my original, more efficient memoization.

### Final Benchmark

```
Parsing input...
benchmarking...
time                 4.411 μs   (4.397 μs .. 4.436 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 4.496 μs   (4.447 μs .. 4.558 μs)
std dev              183.9 ns   (127.1 ns .. 258.4 ns)
variance introduced by outliers: 53% (severely inflated)

================================================================================
Running Part 1...
benchmarking...
time                 847.2 ns   (844.0 ns .. 850.9 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 849.9 ns   (845.7 ns .. 863.0 ns)
std dev              21.62 ns   (4.984 ns .. 41.98 ns)
variance introduced by outliers: 34% (moderately inflated)

ones * threes = 1625
================================================================================
Running Part 2...
benchmarking...
time                 457.5 ns   (454.1 ns .. 461.9 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 454.4 ns   (453.0 ns .. 457.3 ns)
std dev              6.423 ns   (3.530 ns .. 10.55 ns)
variance introduced by outliers: 14% (moderately inflated)

total combinations = 3100448333024
```