# Day 15

This is a fun type of problem which is a sequence problem.  Sometimes there are tricks in calculating sequences such as matrix exponentiation, but none of that applies here.  The next number in the sequence may depend on all previous numbers, so there's a lot of state to track, fortunately it's a pretty simple `Int -> Int` mapping.

The twist is that they ask for the 30,000,000th number in the sequence, which means we have to be at least reasonably efficient.

## Optimization

My first pass at a solution used an `IntMap` to store the last turn each number was spoken.  This sounds pretty reasonable because it gets us what we need and handles the potential sparseness of the numbers.  However, it's slow when calculating millions of numbers.  That first solution took about a second for the first part and over 30 seconds for the second!  Of course, that was enough to get the answers and move on, but I can do better.

A key observation is that, other than the inital numbers, no number spoken can be greater than the current turn count.  That means that if we are going to through _N_ numbers then no number will be greater than _N_.  We can just index into an array!  Even for 30,000,000 numbers, that's 30M words or 120MB of memory, which is acceptable for a modern machine. For comparison, the `IntMap` solution has 392033 entries when going to 30M.

I opted to use an `STUArray` to get unboxed values because the values are also integers.  I also used `Int32` to keep it to four bytes per entry.  These changes made the code fast enough to benchmark without having to leave my desk.  Part one was down to 80μs and part two was down to 1.5s.  I also tried a mutable unboxed vector, which had very similar performance (as expected).

Using unsafe reads and writes was a tiny improvement, about 5%.

One little nuance I ran into: a boxed vector was about 10x slower.  This seemed to have to do with thunks building up in `sayNext`.  I couldn't seem to figure out how to resolve it.  I'd love to know how to debug that in the future and fix it.

```
       playGame.game                                  Main                                 Day15/Main.hs:(59,7)-(61,52)                                 24529          1    1.4    3.0    99.8   99.7
        rememberNumber                                Main                                 Day15/Main.hs:(30,1)-(32,48)                                 24532          6    0.0    0.0     0.0    0.0
        *>                                            Main                                 Day15/Main.hs:24:22-32                                       24545          0    0.9    0.1     0.9    0.1
        >>                                            Main                                 Day15/Main.hs:24:35-39                                       24530          0    0.3    0.0    97.5   96.6
         *>                                           Main                                 Day15/Main.hs:24:22-32                                       24546          0   10.9    7.3    97.2   96.6
          sayNext                                     Main                                 Day15/Main.hs:(40,1)-(49,38)                                 24547          0    0.0    0.0    86.2   89.3
           >>=                                        Main                                 Day15/Main.hs:24:35-39                                       24548          0   69.2   72.8    86.2   89.3
            recallNumber                              Main                                 Day15/Main.hs:(35,1)-(37,45)                                 24552   29999993    5.3    6.4     9.1    7.4
             basicLength                              Data.Vector.Mutable                  Data/Vector/Mutable.hs:88:3-33                               24554   29999993    0.0    0.0     0.0    0.0
             basicUnsafeRead                          Data.Vector.Mutable                  Data/Vector/Mutable.hs:117:3-59                              24555   29999993    0.5    0.9     3.8    0.9
              marray#                                 Data.Primitive.Array                 Data/Primitive/Array.hs:92:5-11                              24556   29999993    0.0    0.0     0.0    0.0
              primitive                               Control.Monad.Primitive              Control/Monad/Primitive.hs:205:3-16                          24557   29999993    3.2    0.0     3.2    0.0
             unwrapHistory                            Main                                 Day15/Main.hs:14:30-42                                       24553   29999993    0.0    0.0     0.0    0.0
            rememberNumber                            Main                                 Day15/Main.hs:(30,1)-(32,48)                                 24558   29999993    5.7    7.6     7.9    9.2
             basicLength                              Data.Vector.Mutable                  Data/Vector/Mutable.hs:88:3-33                               24560   29999993    0.0    0.0     0.0    0.0
             basicUnsafeWrite                         Data.Vector.Mutable                  Data/Vector/Mutable.hs:120:3-65                              24561   29999993    0.5    1.5     2.2    1.5
              primitive                               Control.Monad.Primitive              Control/Monad/Primitive.hs:205:3-16                          24562   29999993    1.7    0.0     1.7    0.0
               marray#                                Data.Primitive.Array                 Data/Primitive/Array.hs:92:5-11                              24563   29999993    0.0    0.0     0.0    0.0
             unwrapHistory                            Main                                 Day15/Main.hs:14:30-42                                       24559   29999993    0.0    0.0     0.0    0.0
            sayNext.newTurn                           Main                                 Day15/Main.hs:47:7-29                                        24551   29999993    0.0    0.0     0.0    0.0
            sayNext.nextNumber                        Main                                 Day15/Main.hs:(43,7)-(46,16)                                 24564   29999993    0.1    0.0     0.1    0.0
```

### Final Benchmark

```
================================================================================
Running Part 1...
benchmarking...
time                 80.21 μs   (79.59 μs .. 81.08 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 79.86 μs   (79.57 μs .. 80.38 μs)
std dev              1.292 μs   (811.5 ns .. 2.220 μs)
variance introduced by outliers: 11% (moderately inflated)

2020th turn starting with [16,1,0,18,12,14,19]: 929
================================================================================
Running Part 2...
benchmarking...
time                 1.537 s    (1.472 s .. 1.649 s)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 1.570 s    (1.542 s .. 1.614 s)
std dev              42.02 ms   (6.927 ms .. 55.29 ms)
variance introduced by outliers: 19% (moderately inflated)

30,000,000th turn starting with [16,1,0,18,12,14,19]: 16671510
```