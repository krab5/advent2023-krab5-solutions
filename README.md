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


### [Dec 9, 2023]

An easy challenge, what am I suppose to do this week-end???

I must admit I have to give it to lazyness (again), saving me when calculations
would be terrible otherwise...

<br>

**Edit:** I was so bored I actually expanded my code base. I re-factored a bit
the parsing library, added new "main makers"... Even did a bit of
documentation.


### [Dec 10, 2023]

An interesting challenge! I think the first part was just to push you to set up
something to get the path, the second part is the real challenge imo.

This is the first challenge where multiple choices are possible in term of data
structures. I could have gone for a graph as a linked structure, but in fact the
input is quite packed, so using a grid/matrix is easier with no overhead.

For establishing if a point is in the inside or outside of the path, I used the
intersection technique: for a given point (not in the path), draw an horizontal
and a vertical line up to the border. Count how many times this line intersect
with the path. If it that number is odd in all four directions (north, south,
east and west), the point is inside.

This works because the path is connected (it makes a loop) and because you have
only two directions for that path (horizontal and vertical).

The only difficulty is to handle the line when it goes over a segment of the
path, i.e.:
```
> ..╔═══╝..
```

In that case, you have to consider the turns at the beginning and end of the
segment. If they are identical, you crossed the path twice; otherwise, you
crossed the path only once :
```
  ABCDEFGHIJKLM
0 ..........║..
1 ..╔═══╗...║..
2 ..║...╚═══╝..
3 ..║..........
```

Intersecting the path to the east at 1A gives 3 points of intersection, and
same thing for 2A. Intersecting the path to the south at G0 gives 1 point of
intersection. The rational is that in the first case, if you were using reals
and were slightly below the border, you would indeed have two points of
intersection (or 0 if you are slightly above, it ends up being the same thing as
far as the algorithm goes). In the second case, no matter if you are slightly
above/below (or in that case left/right) of the path, you have to cross it
exactly once.

<br>

The only thing I am moderately happy about is that it is difficult to argue
about the safety of my code. In fact, I wrote the `Grid` module in a fairly safe
way, but the different parts of algorithms naturally exclude some edge cases.

Typically, the `Pipes` module is not very safe, but it (should be) is used
safely, by virtue of the algorithms' design... Nonetheless, if I had time and
motivation, I would render the code a bit safer, while trying not to incorporate
too much overhead.

<br>

I made quite a good grid library for fun, with needlessly fancy features (the 
program colors tiles that are in the path, that are in the inside, etc.). I
suspect it will be useful again later in the AoC (it always is...).

I will never in my life be thankful enough for Haskell's `vector` and `set`
libraries.


### [Dec 11, 2023]

An easy challenge to start the week. I decided to go with a list of nodes and
then update their coordinates when expanding, with a direct formula. The idea is
that for any _x_, you count the number of columns before that and add the
expansion factor minus 1 for each one (and similarly for _y_ and rows) and there
you go. Of course, you must not forget the minus 1, otherwise you expand one
times too much (had trouble wrapping my monkey brain around that, what a stupid
mistake).


### [Dec 12, 2023]

Oh boy, this challenge. I am not a great fan of combinatorics, this one really
gave me some headaches.

I started with a simple, "naive" algorithm, that generates/count every
solutions. Of course, you get a lot of redundant calculation, but I did not
mind... Until part 2.

Part 2 was simply out of reach with my first algorithm. In fact, the solution is
in the order of 10^14, not a chance I could get there that way.

I tried reasoning with groups instead of single elements, I tried
parallelization (a little bit), I tried early pruning heuristics... In the end,
I manage to devise a divide-and-conquer strategy, and with a few heuristic
tweaks (on the division part in particular), I was able to drag down execution
time below 1 minute.

Not gonna lie, quite proud of it. But the amount of time I spent debugging weird
edge cases and mismatched counts drove me insane.


### [Dec 13, 2023]

Not a terribly interesting challenge. I went to it in a relatively brute-force-y
way (albeit with a few optimisations, which are more coming from how Haskell
works). For the second part my code is a bit meh, because I wanted to show which
position the smudge was. Without that, the code would be much, much cleaner.


### [Day 14, 2023]

Overall a fun challenge. My first implementation for calculating tilting in a
direction was based on immutable vectors and was not very efficient. The part 2
was obviously some sort of cycle finding, but I was afraid the cycle would be
enormous and/or late, and so that the calculation would take ages...

I ended up writing a version of Grid with mutable vectors. They are faster
overall, especially when iterating a lot, and you can always use a ST monad to
make your algorithm even faster.

Eventually though, the cycle is small and early. WHAT A DRAG.


### [Day 15, 2023]

A fun, easy challenge, and exactly the kind of problem I love: some kind of
execution semantics to be applied. I was greatly helped by my now quit developed
parsing library, the rest is just very easy functions. I modularized everything
more as a way to have clean code (which I am fairly okay with tbh).


### [Day 16, 2023]

Another quite fun challenge imo. I love mirror-based problems.

I went out of my way to give a _fancy_ representation for the beams using
unicode framing characters, AND I DO NOT REGRET IT.

I could have gone with a simple boolean grid, parallel search and so on, and I
could have a calculation time under a minute, BUT NO. I WANT THE PRETTY BEAMS.

I think I spent twice as much time setting up pretty printing than writing the
actual algorithm for finding where the beam is going lmao.


