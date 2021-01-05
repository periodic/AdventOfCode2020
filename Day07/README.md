# Day 7

This exercise has two tricky parts to it.  First, the parsing is a bit more complicated than previous exercises because the input is structured as a sentence.  Second, the work itself involves some graph traversal.

The parsing gave me the most trouble.  I think this was only the second question for which I broke out Attoparsec, and I was still rusty with it.  It was easy to accidentally consume too much or not enough input and end up not consuming the whole input.  This is partially due to using `parseOnly` and not bothering to match `endOfInput` on most parsers.  The resulting code was not too complicated, but it certainly had its subtleties.

Once the rules were all parsed, it was a matter of creating a graph with weighted directed edges, where the weights represent the multiplicity of the contained item.  The simplest representation that I'm aware of in Haskell is to use a `Map`.  Since the two parts of the question involve traversing the graph in both directions this had to be a bi-directional map.  I rolled my own since it was relatively easy to do.  I was not aware at the time that there is a `Graph` in `containers`, but it does not seem to support weights.

The algorithm was a pretty standard case of doing reachability exploration, first in the reverse direction and counting then all, then in the forward direction and multiplying the count of the node by the weight of the edge pointing in.

```
Parsing input...
benchmarking...
time                 1.009 ms   (1.006 ms .. 1.012 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.021 ms   (1.012 ms .. 1.043 ms)
std dev              45.47 μs   (14.94 μs .. 89.87 μs)
variance introduced by outliers: 34% (moderately inflated)

================================================================================
Running Part 1...
benchmarking...
time                 140.2 μs   (139.9 μs .. 140.6 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 140.2 μs   (139.9 μs .. 140.8 μs)
std dev              1.246 μs   (726.8 ns .. 2.222 μs)

Total containers for 'shiny gold': 151
================================================================================
Running Part 2...
benchmarking...
time                 17.90 μs   (17.88 μs .. 17.94 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 17.91 μs   (17.89 μs .. 17.93 μs)
std dev              65.12 ns   (43.91 ns .. 104.6 ns)

Total contents for 'shiny gold': 41559
```