# Day 23

When I first saw this problem I thought this would be a perfect place to use `Seq`.  It looks like a sequence after all!

That worked fine for part 1, but broke down pretty hard in part 2.  I looked around and got the tip that this is more like a ring graph than a sequence and so the right way to encode it is to just hold a map from a number to the next number in the sequence.  This makes it trivial to look up numbers by their number for insertion and makes moving numbers around very easy.

The code could be a lot faster.  For example, it could use a mutable vector.  My implementation ran in about 9 minutes on my system, but I hear that coding it in C with a flat array can execute in under a second.

TODO:

- Look for performance improvements by switching to something like a mutable array instead of IntMap.