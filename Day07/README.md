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

I feel like these should be a way to make the graph traversal more abstract so it can be shared, but the graph is not really a container, so it resists being made an instance of classes like `Traversible` and `Foldable` because the nature of the two traversals are so different.  It's possible that it can be thought of as a container of edges, but the issue of handling avoiding revisiting nodes in the first one but not the second is a bit awkward.

Regardless, there is a trick that we can use, which is lazily building a map!  It's like free memoization and simplifies the code.  This means that I have to switch to lazy maps to avoid loops, but it is beautiful in its power, which is only possible with laziness.

Finding all the containers was a matter of doing a graph exploration with a queue and looking for everything reachable, but this is a much more direct method of defining how to define what contains what and then letting the system calculate what it needs.  I worry this would not work if the graph were cyclic, while the former solution was.  That's one reason I went with it originally because I wasn't sure.  The resulting function is quite simple:

```haskell
allContainers :: Bag -> BagGraph -> Set Bag
allContainers source graph =
    Map.findWithDefault Set.empty source containers
    where
        containers = Map.map (\directContainers -> 
            Set.unions . Set.insert directContainers . Set.map (\c -> Map.findWithDefault Set.empty c containers) $ directContainers) 
            (Graph.reverse graph)
```

It's also not much slower than the original solution.

```
Running Part 1...
benchmarking...
time                 166.0 μs   (165.7 μs .. 166.3 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 166.0 μs   (165.9 μs .. 166.2 μs)
std dev              447.9 ns   (346.0 ns .. 598.3 ns)

Total containers for 'shiny gold': 151
```

The same trick can be applied to the other problem as well:

```haskell
countContents :: Bag -> BagGraph -> Int
countContents source graph =
    Map.findWithDefault 0 source bagSizes
    where
        bagSizes = Map.map (
            (+1) . sum . List.map (\(count, bag) -> count * Map.findWithDefault 0 bag bagSizes))
            (forward graph)
```

And again, the runtime does not change appreciably.

```
Running Part 2...
benchmarking...
time                 17.45 μs   (17.37 μs .. 17.57 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 17.53 μs   (17.43 μs .. 17.77 μs)
std dev              456.1 ns   (110.9 ns .. 797.7 ns)
variance introduced by outliers: 28% (moderately inflated)
```

In the end, I really liked how I was able to utilize the laziness to come up with a very elegant solution.  In many of these exercises I made everything as strict as possible out of fear of performance hits.  That made me blind to places where laziness really is the best solution!

Credit goes to [mstksg](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day07.md) for the idea.
