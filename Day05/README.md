# Day 5

This was another quick one.  The biggest challenge is just in seeing that the input lines are numbers written out in obscured binary.  That's relatively easy to extract.  After that it's just a matter of scanning the input.

One interesting change I made when I revisited it was in the way I found the missing seat.  At first I was just doing a fold and using `Either` to track whether I'd found the result or still need to keep going.  That worked fine, but it always scans the whole list instead of just stopping when something is found.  I switched it to use `Data.List.find` after zipping the list with its own `tail`. and it's both slightly faster and the code is simpler.  Either way, the input has to be sorted, which dominates the run time.

```
================================================================================
Running Parsing...
benchmarking...
time                 9.411 ns   (9.375 ns .. 9.482 ns)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 9.608 ns   (9.469 ns .. 9.952 ns)
std dev              709.6 ps   (319.1 ps .. 1.343 ns)
variance introduced by outliers: 86% (severely inflated)

================================================================================
Running Part 1...
benchmarking...
time                 2.092 μs   (2.072 μs .. 2.123 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 2.080 μs   (2.074 μs .. 2.096 μs)
std dev              29.95 ns   (12.62 ns .. 56.09 ns)
variance introduced by outliers: 13% (moderately inflated)

Maximum seat ID: 855
================================================================================
Running Part 2...
benchmarking...
time                 96.00 μs   (95.70 μs .. 96.33 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 95.75 μs   (95.62 μs .. 95.97 μs)
std dev              574.8 ns   (403.8 ns .. 859.4 ns)

Missing seat ID: Just 552
```