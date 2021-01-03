# Day 3

The most difficult part of this exercise was just the parsing.  Making a grid into an easily indexed structure is actually a bit annoying, particularly if you want to use an array, as I did initially.  It seemed like the most appropriate structure, but I switched to nested vectors because they are easier to construct and the total memory size is very small anyway so they all fit in the cache and don't seem to have any performance impacts.

I also initially created my own two-element vector class, but later I discovered the `linear` package which handles vectors quite well.

The exercise of counting the number of trees is pretty simple because we can just use `iterate` on vector addition and then keep pulling elements until something goes out of bounds and returns `Nothing`.

```
Parsing input...
benchmarking...
time                 824.8 μs   (822.4 μs .. 829.3 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 832.4 μs   (827.7 μs .. 840.7 μs)
std dev              21.28 μs   (13.66 μs .. 31.68 μs)
variance introduced by outliers: 15% (moderately inflated)

================================================================================
Running Part 1...
benchmarking...
time                 11.87 μs   (11.80 μs .. 11.98 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 11.87 μs   (11.83 μs .. 11.97 μs)
std dev              211.4 ns   (102.0 ns .. 358.4 ns)
variance introduced by outliers: 16% (moderately inflated)

Trees on (1, 3): 173
================================================================================
Running Part 2...
benchmarking...
time                 40.67 μs   (40.43 μs .. 40.92 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 40.66 μs   (40.45 μs .. 41.17 μs)
std dev              1.025 μs   (502.6 ns .. 2.041 μs)
variance introduced by outliers: 24% (moderately inflated)

Tree product: 4385176320
```