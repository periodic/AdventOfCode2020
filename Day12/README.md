# Day 12

This problem is another case of a list traversal.  Once all the instructions are parsed, the goal is to traverse the list and maintain a little state along the way.  The difference between the two parts is that the second part has a little extra state and the rules are a little different, but ultimately they are very similar.

The two parts basically boil down to defining:

* A type for the state.
* A function that maps an instruction to a state update.
* Initial state.

Beyond that, the actual execution becomes very simple, as it's mostly a bit of vector manipulation.

## Optimization

The first thing I noticed when looking at the benchmarks is that the second part was a lot slower than the first part, almost 30x slower!  Just glancing at the code there isn't an obvious part that is making it more complicated. 

At first I thought it's probably in how I'm dealing with the rotation, because that was doing some extra steps, but cleaning that up didn't do much.

The real problem was that I was modeling the waypoint movement as repeated applications of movement, but since they are all in the same direction it's faster to do it by scaling the vector first.  I did that and switched everything over to using vectors form the linear module at the same time and now both parts take about 15μs.

### Final Benchmark

```
Parsing input...
benchmarking...
time                 162.6 μs   (161.3 μs .. 164.5 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 163.2 μs   (162.7 μs .. 164.0 μs)
std dev              2.031 μs   (1.285 μs .. 2.956 μs)

================================================================================
Running Part 1...
benchmarking...
time                 15.53 μs   (15.39 μs .. 15.67 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 15.38 μs   (15.33 μs .. 15.49 μs)
std dev              245.4 ns   (161.7 ns .. 351.0 ns)
variance introduced by outliers: 13% (moderately inflated)

Total Manhattan distance after ferry movement: 938
================================================================================
Running Part 2...
benchmarking...
time                 15.36 μs   (15.21 μs .. 15.54 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 15.35 μs   (15.28 μs .. 15.46 μs)
std dev              285.7 ns   (179.0 ns .. 509.4 ns)
variance introduced by outliers: 17% (moderately inflated)

Total Manhattan distance after waypoint movement: 54404
```