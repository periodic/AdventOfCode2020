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

On the second part, I already mentioned that it's possible to squeeze out a little bit more performance by hand-rolling the memoization, but it's a bit more wordy than I think it worth it for only a 2x speed-up.

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
time                 10.19 μs   (10.13 μs .. 10.26 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 10.23 μs   (10.16 μs .. 10.36 μs)
std dev              286.9 ns   (93.66 ns .. 484.7 ns)
variance introduced by outliers: 32% (moderately inflated)

total combinations = 3100448333024
```