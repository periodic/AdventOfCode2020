# Day 6

This problem really boils down to set operations.  Each group is a set of answers that are unioned in part 1 and intersected in part 2.

My first solution was very quick.  Strings are implemented as lists of characters, so they can easily be unioned or intersected with utilities from `Data.List`.  `intersect` is first-class, and union  can be done with `nub . concat`.  This leads to very reasonable performance for the small input, so it's easy to stop there.

```
================================================================================
Running Parsing...
benchmarking...
time                 48.92 ns   (48.53 ns .. 49.45 ns)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 48.57 ns   (48.41 ns .. 48.86 ns)
std dev              762.9 ps   (514.7 ps .. 1.157 ns)
variance introduced by outliers: 20% (moderately inflated)

================================================================================
Running Part 1...
benchmarking...
time                 892.9 μs   (884.2 μs .. 902.9 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 906.7 μs   (901.2 μs .. 913.3 μs)
std dev              20.15 μs   (18.21 μs .. 21.72 μs)
variance introduced by outliers: 12% (moderately inflated)

Sum of unique answers per group: 6782
================================================================================
Running Part 2...
benchmarking...
time                 2.062 ms   (2.049 ms .. 2.083 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 2.052 ms   (2.042 ms .. 2.069 ms)
std dev              41.63 μs   (26.33 μs .. 72.51 μs)

Sum of common answers per group: 3596
```

But lets see what we can do with proper sets and parsing!  It would be fun to see how the quick answer and the more through answer compare.

First, I can switch the parsing over and even leave the same type signatures because combinators like `sepBy` produce lists and the main consumer of input is `letter :: Parser Char`.

This was a bit slower.  Not a lot, but certainly more than just splitting strings.

```
Parsing input...
benchmarking...
time                 1.131 ms   (1.124 ms .. 1.142 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.145 ms   (1.141 ms .. 1.150 ms)
std dev              14.42 μs   (10.38 μs .. 20.33 μs)
```

How about if we use sets? Turns out, to no one's surprise, that union and intersection on sets is more efficient than on lists, even with the small lists we have in this exercise.

```
Running Part 1...
benchmarking...
time                 468.4 μs   (466.5 μs .. 471.1 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 469.4 μs   (468.3 μs .. 472.7 μs)
std dev              6.044 μs   (2.554 μs .. 11.69 μs)

Sum of unique answers per group: 6782
================================================================================
Running Part 2...
benchmarking...
time                 575.2 μs   (573.1 μs .. 577.6 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 577.7 μs   (574.4 μs .. 593.1 μs)
std dev              19.70 μs   (3.414 μs .. 44.40 μs)
variance introduced by outliers: 26% (moderately inflated)
```

In short: Basic string parsing wins, but sets still beat out lists even for small datasets like this one.

A further optimization could be made by noting that the input is just letters, of which there are 26, so we could go further to optimize the set by representing it as a bitmap in an Int.  That might increase the run-time of the parsing, but would reduce the two parts because union/intersection are just bitwise or/and operations.