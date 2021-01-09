# Day 10

These problems are basically just a few list traversal algorithms.

The first one isn't too hard if you just remember to add the start and end elements. Theres a nice trick to getting all the diffs by doing

```haskell
differences :: [Int] -> [Int]
differences joltages =
    zipWith (-) (tail joltages) joltages
```

From there you can just just fold it into some counts, build a map out of it, or even just filter it twice.  Either way, it's pretty fast.

The se