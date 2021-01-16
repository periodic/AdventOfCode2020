# Day 16

This question has a small set-up, but then turns into a constraint satisfaction problem, a common one in exercises like these.  It ended up showing up again on Day 21.

I first approached this by defining a structure to cover the ranges, as a semi-group and monoid, only to look at the data later and realize that the input only has two ranges per field, and not an arbitrary amount.  Another case of reading the spec and seeing the need for flexibility where there wasn't any.  In the end I don't think it hurt much in the way of performance and the ranges were useful.

I also noticed that the constraint solving that is required can be done by just fixing fields that have just one option, and then the whole solving doesn't look like it actually _needs_ any backtracking.  However, I wouldn't want to assume that for general input.

Still, it's important to solve the most constrained fields first, because otherwise the complexity can explode with lots of backtracking.

## Optimization

The initial run without any extra optimization was pretty reasonable, 500μs to parse, 135μs for the first part and 1,400μs on the second part.  None of these are perceptible when running the program.

Doing a little profiling turns up a few candidates.  `inRange` is called a lot when figuring out which fields can match which rule, also when filtering out the invalid tickets that have values that don't match any rule.  That accounts for about 25% of execution time for the entire program.  Reducing the amount if repeated checks is going to be key, unfortunately, I can't seem to figure out a way to make that any faster.

### Final Benchmark

```
Parsing input...
benchmarking...
time                 528.4 μs   (527.2 μs .. 530.1 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 535.1 μs   (531.4 μs .. 544.6 μs)
std dev              18.30 μs   (6.636 μs .. 36.73 μs)
variance introduced by outliers: 26% (moderately inflated)

================================================================================
Running Part 1...
benchmarking...
time                 145.0 μs   (144.0 μs .. 146.4 μs)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 147.0 μs   (146.2 μs .. 148.0 μs)
std dev              2.928 μs   (2.346 μs .. 3.792 μs)
variance introduced by outliers: 14% (moderately inflated)

Ticket scanning error rate: 26941
================================================================================
Running Part 2...
benchmarking...
time                 1.311 ms   (1.298 ms .. 1.324 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.304 ms   (1.300 ms .. 1.308 ms)
std dev              14.32 μs   (10.65 μs .. 22.18 μs)

Product of departure fields: 634796407951
```