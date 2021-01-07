# Day 8

This question's a pretty fun one and one of my favorites: implementing a small language or VM.  It's a nice and small version that still tests a lot of the principles without a lot of complication.

The parsing is straight forward. There are a few different commands to parse and they are in an ordered list.  One thing I did not anticipate is that the no-op commands still need to remember their argument for part two.

I opted to store the program with an ADT for instructions and put them all in a `Vector`.  This gave me numeric indexing and easy matching on instruction type.

By far, the most interesting part is running the program, but even here it's a pretty straightforward system. I opted to implement it with small state monad.

Something I would consider doing in the future is to separate out the loop checking from the evaluation. This would mean writing functions to step the program forward, and then running it using something that remembers those states and iterates until it repeats.

## Optimization

One optimization I missed on my first time through is that there is an `IntSet` that is much more efficient than `Set Int`.  Using that to track visited instructions, literally just two lines, reduced part 1 from 29μs to 11μs and part 2 from 8ms to 3ms.

Finally, I notice that I'm still returning programs that aren't modified! That means I was testing programs for loops that I know are going to loop.  Those can be filtered out with a quick `mapMaybe` and that cuts the time of part 2 down from 3ms to 1.3ms. That implies it was almost half wasted work!

```
Parsing input...
benchmarking...
time                 130.7 μs   (130.4 μs .. 131.3 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 131.1 μs   (130.8 μs .. 131.4 μs)
std dev              981.1 ns   (834.7 ns .. 1.281 μs)

================================================================================
Running Part 1...
benchmarking...
time                 10.01 μs   (10.00 μs .. 10.02 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 10.02 μs   (10.02 μs .. 10.03 μs)
std dev              30.58 ns   (22.44 ns .. 45.59 ns)

Looped with value 2014
================================================================================
Running Part 2...
benchmarking...
time                 1.265 ms   (1.256 ms .. 1.279 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 1.268 ms   (1.263 ms .. 1.277 ms)
std dev              23.60 μs   (11.39 μs .. 36.52 μs)

Finished with value 2251
```