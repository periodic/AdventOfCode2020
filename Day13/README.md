# Day 13

This question is mostly a math question, but with enough computation required to be worth programming.

The parsing was nothing interesting, though the `x` entries were easy to ignore in the first part and were important in the second part.

The first question is basically about finding _B - (B_ mod _k)_ for each buss and then choosing the B with the minimum value.  That gets us the soonest bus that arrives after we did.

The second part is more interesting.  Searching the space will take a looooong time.

First, we can see that we want to find the least t such that for all busses _i_ with period _B_ we have  _B · k - i = t_. Once we have found a time it true for _B₁_, it will be true for that bus at intervals of _B₁_.  This means that if we have found a solution for one bus and want to add another bus, we only need to consider multiples of _B₁_.  Further, once we have added that bus it will be true for both busses at an interval of _B₁ · B₂_.  That means instead of trying to find a solution for all busses at once, we can basically add one bus at a time, and each time we reduce the search space by a factor the size of the index of the bus.

## Optimization

There wasn't much optimization to do here since it's mostly about the math.

### Final Benchmark

```
Parsing input...
benchmarking...
time                 7.767 μs   (7.733 μs .. 7.813 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 7.766 μs   (7.732 μs .. 7.874 μs)
std dev              188.6 ns   (68.78 ns .. 376.3 ns)
variance introduced by outliers: 27% (moderately inflated)

================================================================================
Running Part 1...
benchmarking...
time                 746.7 ns   (736.1 ns .. 757.5 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 739.9 ns   (735.3 ns .. 746.9 ns)
std dev              18.61 ns   (13.79 ns .. 29.21 ns)
variance introduced by outliers: 33% (moderately inflated)

Next bus is 607 in 5
Product of those is 3035
================================================================================
Running Part 2...
benchmarking...
time                 34.42 μs   (33.94 μs .. 34.85 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 34.32 μs   (34.06 μs .. 34.79 μs)
std dev              1.138 μs   (707.7 ns .. 1.775 μs)
variance introduced by outliers: 36% (moderately inflated)

First time of sequential departures: 725169163285238
```