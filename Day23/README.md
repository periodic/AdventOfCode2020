# Day 23

When I first saw this problem I thought this would be a perfect place to use `Seq`.  It looks like a sequence after all!

That worked fine for part 1, but broke down pretty hard in part 2.  Just doing 100 moves with the full million elements took more than a few seconds, so scaling up to ten million moves would be infeasible.  I looked around and got the tip that this is more like a ring graph than a sequence and so the right way to encode it is to just hold a map from a number to the next number in the sequence.  This makes it trivial to look up numbers by their number for insertion and makes moving numbers around very easy.

This worked, to a degree.  It ran relatively fast on smaller iterations.  It took about six seconds on 100,000.  I just started it up on the full iteration count and walked away for something.  By the time I got back it had completed in about 9 minutes.

But I think the code could be a lot faster.  For example, it could use a mutable vector.  I heard that coding it in C with a flat array can execute in under a second.

I updated it to use a mutable unboxed array.  This was suprisingly easy now that I get how ST works.  This brought the runtime down to seven seconds.  If I turn off threading, it's down to 1.3 seconds.  I'm pretty happy with a 40,000% speed up.
