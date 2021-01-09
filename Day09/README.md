# Day 9

This question is basically a linear scan of a sequence, with a little computation thrown in.  The first part involves testing a condition on subsets of 26 numbers, while the second part involves searching for a subset that satisfies another condition.  These are two things that are very easy to do on computers and so are relatively quick even if they aren't done in the most efficient manner.

The first part has two algorithms: scanning the list and evaluating if the condition holds.  I opted to use a `Vector` to hold the sequence because the main operation I need is the ability to index and get sub-sequences, both of which are very efficient on `Vector`.  My first pass at the evaluation used a brute-force comparison of all pairs, looking for a pair that summed to the desired number.  This is just like the code from Day 1!  I later converted it to use a similar set-based approach and cut the run-time in half.

The second part involves looking for a contiguous subset that sums to a target number (other than that number itself). There's an easy algorithm for this that just keeps a start and end and increases the end if the current sum is too low and increases the start if the current sum is too high.  It's just a linear scan of the input.

Even with all the inefficiencies I had, my initial solution ran in a few milliseconds, so I took my stars and moved on.

## Optimization

### Part 1

The first optimization I alluded to above.  Instead of generating all pairs of the 25 numbers, _O(n²)_, we can put them in a set and then for each number look if the difference between the number and the target are in the set.  This reduces it to _O(n log n)_, which seems to cut the total time by about half from 942µs to 480µs.

This makes me realize that I'm doing a lot of extra work to build that set every time.  Each time we move to the next index, the set of interesting values adds one number and removes one number. Maintaining this `IntSet` as we go drops the runtime of the first part to only 188µs.

This is technically not valid though!  There may be duplicates, but this does not happen on this input.

A multi-set is easy to implement through a map.  I tried that, using an `IntMap` to back it.  It worked, but was slightly slower at 362µs, though that may be due to lack of optimization on the data-structure.  I'm going to leave it in as more correct, even though the `IntSet` worked fine on this input.

```haskell
newtype MultiSet = MultiSet {
    toMap :: M.IntMap Int
}

empty :: MultiSet
empty =
    MultiSet M.empty

insert :: Int -> MultiSet -> MultiSet
insert k =
    MultiSet . M.alter (Just . maybe 1 (+1)) k . toMap

remove :: Int -> MultiSet -> MultiSet
remove k =
    MultiSet . M.update (\n -> if n > 1 then Just (n - 1) else Nothing) k . toMap

member :: Int -> MultiSet -> Bool
member k =
    M.member k . toMap

fromList :: [Int] -> MultiSet
fromList =
    foldr insert empty

toList :: MultiSet -> [Int]
toList =
    M.keys . toMap
```

### Part 2

The first thing that stands out is that I initially was recomputing the sum each time.  Computing the sum of a fixed range on a vector is pretty fast, but maybe it can be faster if we keep a running sum.  It's when expanding the subset we add a number, and when contracting the subset we subtract it.  Pretty simple.

This tiny optimization improved the runtime of the second part from 620µs to 1.8µs!! Wow!

### Final Benchmark

```
Parsing input...
benchmarking...
time                 60.30 μs   (59.92 μs .. 60.66 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 60.30 μs   (59.98 μs .. 61.31 μs)
std dev              1.642 μs   (922.5 ns .. 3.170 μs)
variance introduced by outliers: 26% (moderately inflated)

================================================================================
Running Part 1...
benchmarking...
time                 399.3 μs   (396.7 μs .. 402.5 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 395.3 μs   (393.9 μs .. 397.7 μs)
std dev              5.829 μs   (3.942 μs .. 9.844 μs)

Invalid number: 31161678
================================================================================
Running Part 2...
benchmarking...
time                 1.827 μs   (1.824 μs .. 1.830 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.831 μs   (1.823 μs .. 1.862 μs)
std dev              45.23 ns   (14.37 ns .. 98.89 ns)
variance introduced by outliers: 31% (moderately inflated)

Sum of smallest and largest: 5453868
```