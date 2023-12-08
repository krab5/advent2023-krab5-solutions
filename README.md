# advent2023-krab5-solutions

My personal solutions for the [Advent of Code 2023](https://adventofcode.com/).

## Captain's log

### [Dec 1, 2023]

I don't know yet if it will be profitable for the remainder of the AoC but I
wrote a small parsing libraries using parser combinators (nothing too fancy
really). Honestly I would say I spent more time writing the module than
computing the solution but that would be a lie (first because it really did not
take that much time to write the lib and second because PART 2 TOOK WAY TOO MUCH
TIME FFS).

The catch with part two is that letters may be used in two number name. I didn't
even envision that fact to be possible, as my monkey brain was in "this is a
parsing exercise" mode.

To be clear, I though a stream such as `twone...` would yield the result 
`2ne...` (`two` parses and yield `2`, there remains `ne...` in the stream). But 
in fact, it shall give the result `21...`.

I addressed that by allowing a parser to jump back. So in fact the stream
`twone...` parses `two` as `2` and then puts the stream back at `wone...`. This
takes care of any size of covering (i.e., if words have more than 1 character in
common), although for number from 0 to 9 this is not useful.


### [Dec 2, 2023]

I knew writing a full on parsing library would be useful, as the main problem
with today's challenge is getting information from the file. After that, it's
mainly combinatorics and treatments on lists.

I had fun having parsers that return functions (looking almost like CPS
processing), I think it is a good idea, better than my initial solution where I
store `(Color,Int)` pairs and then look for each colors in lists.


### [Dec 3, 2023]

This challenge involves my nemesis: _arithmetic_. I always have troubles
handling rectangles in grids.

I made a small generic module for rectangles, having done the AoC for a few
years now, I know this will come in handy. 

My initial solution did not work for size reasons. If you have `ABC` in a grid,
is the height 1 or 0? For this solution I decided to go with 0. We will say that
a rectangle is defined by its corners inclusively (so `ABC` is the rectangle
between (x, y) and (x+2,y+0), of size (2,0)).

Other than that, my solution is a bit messy and not very "smart". In particular,
I browse through every elements all the time, while I could have written a
smarter algorithm that only investigates elements that are "close by". Laziness
(both mine and Haskell's) is to blame for that.


### [Dec 4, 2023]

A fun challenge overall. I had the opportunity to extend the parsing library,
and I found that my encoding was efficient-ish. I had troubles understanding the
second part of the problem. I was afraid I had to add new cards at the end of
the list (enqueue cards to scratch, in other words), which would have been a bit
of an issue. But it's day 4, and Monday, so in fact it is fairly simple.


### [Dec 5, 2023]

I think I cooked far, far too much for this challenge, but I wanted to get a few
things done. First, I wanted to use my parser library and see how it pans out
with these kind of syntax (with significant newlines), and turns out it is quite
good. Of course, my parser is way too generic for that challenge, but how well.

Second, I smelled the first instance of my _other_ nemesis: _intervals_ (for
reasons quite similar to arithmetic problems). I decided to implement an
efficient "range set" which is a list of interval that is sorted and where no
two interval intersect (representing an union of disjoint intervals).

Not gonna lie, writing extraction (intersection + set difference) was a little
bit cumbersome. But in the end, it works, and it works well.

I almost had an integer problem I think, but in that case I would have gone to
Integer and call it a day (so convenient).


### [Dec 6, 2023]

Today we do second degree polynomial equation solving (yaaaaaay~). There is so
little interest in doing so I decided to go contrarian and do it with
non-deterministic list monad/applicative.

To be fair, a systematic solution is: given max time N and record k, solve for p
the equation p * (N - p) >= k. This boils down to finding the roots of
- p² + N p - k. Assuming such roots exist and are pmin and pmax, then the number
of integers that are a better score than k is (pmax - pmin + 1).

So, instead, we can do state-space exploration with the list monad, which is
much, much funnier. I also wrote a small function to concat integers, because I
didn't want to touch the parser.

All an all a bit of a disappointing challenge; what will I do for the remainder
of the day. Work?? Urgh.


### [Dec 7, 2023]

A fun challenge overall, but my code does not bare a lot of subtlety I think
(there is nothing particularly "smart" about it, save maybe for the way I have
cards ordered by value but still keep the sequencing).

Part 2 was more a question of thinking about different cases, but the cases are
not two weird if you are attentive (I think).


### [Dec 8, 2023]

> What if the map isn't for people - what if the map is for ghosts?

What a perfectly normal thing to think (look what a career in IT does to a
person).

The challenge was quite fun, typically one where the solution is not
particularly complex, but where you need to be (a tiny bit) clever.

Part 2 uses MAH BOY THE LCM OH YAAAAAAAASSSSSS love to see modular arithmetic.

Just for fun I tried to brute-force it by actually calculating the whole path to
see what would come of it. After 20 minutes there was nothing, and considering
the shear number of digits of the answer, not surprising.

Also, I am happy I ended up using Haskell, because I feel like we will see more
and more big nums, which Haskell support natively (I did not want to delve in
big num libraries in other languages tbh).





