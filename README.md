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


