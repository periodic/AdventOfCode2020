# Day 25

This one pretty quickly stood out as something I should remember from my old cryptography classes.  I thought that there might be a tricky way to calculate things quickly, but since the modulus is only in the tens-of-millions I figured it shouldn't be too hard to brute force.  I was worried about what the second part would throw at me.  Thankfully it was very easy.  I have learned my lesson not to try to anticipate it too much and do premature optimization.

I probably went overboard with the types on this one.  It's nice and clear, but they were mostly unnecessary because the modulo arithmetic only mattered on exactly one line.