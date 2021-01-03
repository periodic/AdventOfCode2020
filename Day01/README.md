# Day 1

One thing that does stand out about this problem is that it's recursive. To
find _N_ elements that sum to a target number, _T_, you can try each element
x and then check if there are _N - 1_ elements that sum to _T - x_. The case
of _N = 1_ is just set inclusion.

This question seems pretty simple: just find all the pairs and triples and
check which one sums to the value we want. It's very easy to implement this
with tails, to at least avoid redoing work by looking at pair we've already
seen. That was my first solution and it worked fast enough that I didn't
think much of it. The whole thing ran in about 1ms. However, it's still
_O(n^2)_ for part 1 and _O(n^3)_ for part 2.


I looked a little deeper and saw that we could use a set to at least make
that last check a lot more efficient. This should use a sort of tree and so
we can reduce the complexity by a factor of _N / lg N_. Doing this reduced the run time to a few microseconds.

```
Running Part 1...
benchmarking...
time                 63.73 ns   (63.57 ns .. 63.92 ns)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 63.59 ns   (63.44 ns .. 63.96 ns)
std dev              703.0 ps   (204.8 ps .. 1.197 ns)
variance introduced by outliers: 11% (moderately inflated)

Just (103,1917)
Running Part 2...
benchmarking...
time                 8.014 μs   (7.979 μs .. 8.062 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 8.019 μs   (7.999 μs .. 8.050 μs)
std dev              84.39 ns   (64.77 ns .. 129.4 ns)

Just (232,443,1345)
```