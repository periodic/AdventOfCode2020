# Day 4

This question is 99% parsing.  The goal is to figure out what is valid and what isn't in a long list of elements.  In the spirit of parsing over validation, I try to put as much information about the validity in the types.  At first that meant just having a `Maybe` for each value to represent if it is there or not, but then the second part introduced a twist in that even the present values could be invalid.  This lead me to introduce a `Value` type that can be valid, invalid or missing.

The next tricky part is that the fields can all be out of order or not there at all.  This isn't something that is easy to represent in an applicative parser because it wants its arguments in a specific order.  The solution is to have the parser return functions which do the updates and then just apply them all to an empty passport, thus filling in each field as it is found.

Another issue I ran into is when the value is invalid, but has a valid value as a prefix.  This caused the valid-value parser to succeed, but leave extra characters on the input that couldn't be matched.  I added a small look-ahead to make sure that a valid value parses to the end of the word.

The performance of this exercise isn't too interesting because it is all in the parsing.  Fortunately, Attoparsec is very fast and I didn't have worry about it.

```
Parsing input...
benchmarking...
time                 1.330 ms   (1.315 ms .. 1.355 ms)
                     0.998 R²   (0.995 R² .. 1.000 R²)
mean                 1.330 ms   (1.322 ms .. 1.345 ms)
std dev              37.26 μs   (15.12 μs .. 60.14 μs)
variance introduced by outliers: 16% (moderately inflated)

Total number of passports: 296
================================================================================
Running Part 1...
benchmarking...
time                 2.104 μs   (2.099 μs .. 2.113 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 2.123 μs   (2.111 μs .. 2.148 μs)
std dev              54.90 ns   (32.75 ns .. 99.13 ns)
variance introduced by outliers: 32% (moderately inflated)

Number of passports with required fields: 239
================================================================================
Running Part 2...
benchmarking...
time                 1.976 μs   (1.953 μs .. 2.007 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 1.971 μs   (1.956 μs .. 2.004 μs)
std dev              70.27 ns   (40.53 ns .. 116.2 ns)
variance introduced by outliers: 48% (moderately inflated)

Number of valid passports: 188
```