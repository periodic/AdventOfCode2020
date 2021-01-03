# Day 2

This was the first exercise for which I thought I would be worth breaking out a real parser and got me reacquainted with Attoparsec.  The parsing could be done with a few splits as it isn't very complicated.  Even a regular expression would work.  However, I love using applicative parsers.  Ultimately it is so fast that it doesn't really matter.

The exercise itself was fairly straightforward: there's a list of passwords and we want to just count the number that are valid.  The functions for checking were fairly easy to write, though I don't like that I ended up using `Data.Text.index` despite it's applicability.

```
Parsing input...
benchmarking...
time                 330.2 μs   (325.2 μs .. 334.5 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 324.1 μs   (322.7 μs .. 326.2 μs)
std dev              5.889 μs   (4.041 μs .. 8.565 μs)
variance introduced by outliers: 10% (moderately inflated)

================================================================================
Running Part 1...
benchmarking...
time                 65.83 μs   (65.73 μs .. 65.97 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 66.20 μs   (66.01 μs .. 66.75 μs)
std dev              992.5 ns   (350.0 ns .. 1.990 μs)

569
================================================================================
Running Part 2...
benchmarking...
time                 23.28 μs   (22.63 μs .. 24.02 μs)
                     0.994 R²   (0.992 R² .. 0.996 R²)
mean                 24.12 μs   (23.62 μs .. 24.65 μs)
std dev              1.834 μs   (1.622 μs .. 2.251 μs)
variance introduced by outliers: 76% (severely inflated)

346
```