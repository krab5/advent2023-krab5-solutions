module Range where

import Data.List (intercalate)

-- | A range is identified by its starting point and its "length" or _span_.
-- A range is a set of numbers between `range_start` (included) and `range_start + range_span` (exluced).
data Range = Range { range_start :: Int, range_span :: Int }

-- | The end point of the range (the first number on the right that is outside of the range
range_end :: Range -> Int
range_end r = range_start r + range_span r

-- | Test if a range is empty. A range is empty when its span is negative or zero.
empty_range :: Range -> Bool
empty_range = (<= 0) . range_span

-- | Make a range from its starting point and its ending point (excluding the latter)
mk_rangex :: Int -> Int -> Range
mk_rangex from to = Range { range_start = from, range_span = to - from }

-- | Tests if two ranges intersect.
intersect :: Range -> Range -> Bool
intersect r1 r2 = not (range_end r1 <= range_start r2 || range_end r2 <= range_start r1)

-- | `Show` instance for ranges, represented as `[x-y[` (as an interval, closed on the left and 
-- open on the right
instance Show Range where
  show r = "[" ++ (show $ range_start r) ++ "-" ++ (show $ range_end r) ++ "["

-- | A set of ranges (represented as a sorted list of non-spanning ranges).
--
-- The internal representation enforces very strong invariants, that makes computation possible and
-- efficient. In particular, we have that, for any two consecutive elements [x1-x2[,[y1-y2[ of the range
-- set, the intersection is empty and x2 < y1.
newtype RangeSet = RangeSet { _content :: [Range] }

-- | `Show` instance for the rangeset.
instance Show RangeSet where
  show (RangeSet rs) = "{" ++ intercalate " ∪ " (map show rs) ++ "}"

-- | Offset all intervals of the range set by a given amount.
offset_all :: RangeSet -> Int -> RangeSet
offset_all rs off = 
    RangeSet { _content = map (\r -> r { range_start = range_start r + off }) $ _content rs }

-- | The empty range set
empty_set :: RangeSet
empty_set = RangeSet []

-- | Test is the range set is empty
is_empty :: RangeSet -> Bool
is_empty = null . _content

-- | Insert a range in the range set, collapsing what needs to be collapsed and enforcing the
-- invariants of the interal representation.
insert :: Range -> RangeSet -> RangeSet
insert r (RangeSet rs) =
    RangeSet $ insert_ r rs
    where insert_ r [] = [r]
          insert_ r l@(x:xs)
              -- If the range is empty, adding it to the range set does nothing
              | empty_range r = l
              -- If the range is smaller than the current range, then adding it does nothing
              | range_start r >= range_start x && range_end r <= range_end x = l
              -- Since ranges are sorted, if the start of the range is greater than the end of the current
              -- range, then we need to insert in a further range
              | range_start r > range_end x = x:(insert_ r xs)
              -- If (after all these guards), the end of the range is before the start of the current
              -- range, then the range is already disjoint from others, and we can safely append it here
              | range_end r < range_start x = r:l
              -- In any other way, the range is "spanning": it starts in the current range and extends
              -- beyond it. We need to collapse as many ranges as necessary to enfoce the invariant.
              | otherwise = 
                  eat (min (range_start x) (range_start r)) (range_end r) l
          -- In eat, low is the lower bound of the range being added, and up is the upper bound
          eat low up [] = [mk_rangex low up]
          eat low up l@(x:xs)
              -- Initially, low is either the start of r or the start of x; up will be growing as
              -- ranges are being "eaten"
              | up >= range_start x = eat low (max up $ range_end x) xs
              | otherwise = (mk_rangex low up):l

-- | Compute the union of two range sets. This function preserves the invariants.
union :: RangeSet -> RangeSet -> RangeSet
union r1 r2
    | is_empty r1 = r2
    | is_empty r2 = r1
    | otherwise = foldr insert r2 (_content r1)

-- | Compute the extraction of the given range from the range set. This function returns a pair
-- which first element is the range set representing elements that have been effectively removed, and
-- which second element is the range set that correspond to the input with any element of the given range
-- revmoed.
--
-- In other words, the first is the intersection between the rangeset and the range, and the second is
-- the difference between the rangeset and the range.
--
-- Both rangeset enforce the invariants.
extract :: Range -> RangeSet -> (RangeSet,RangeSet)
extract r (RangeSet rs) =
    let (ex,rem) = extract_ r rs in (RangeSet ex, RangeSet rem)
    where extract_ r [] = ([],[])
          extract_ r l@(x:xs)
              -- If the range is empty, the range set is left untouched and no element has been removed
              | empty_range r = ([],l)
              -- Idem if the end of the range is below or equal to the start of the current range
              -- (the range is "out of range", so to say)
              | range_end r <= range_start x = ([],l)
              -- If the start of the range is greater than the end of the current range, we continue
              -- browsing (because range set is sorted)
              | range_start r > range_end x = let (ex,rem) = extract_ r xs in (ex,x:rem)
              -- If the range covers the current range entirely, we remove it, and mark it as such,
              -- then continue the browsing (in case the range extends over the next range in the set).
              | range_start r <= range_start x && range_end r >= range_end x = 
                  let (ex,rem) = extract_ r xs in (x:ex,rem)
              -- If the range starts before the current range and ends before the current range, we
              -- shrink the current range, and note the removed section as such.
              -- This correponds to the following situation:
              --         [____r____]
              --               [_____x_____] ...
              -- => rem=            [__x'__] ...
              --    ex =       [____]
              | range_start r <= range_start x && range_end r < range_end x =
                  ([mk_rangex (range_start x) (range_end r)], (mk_rangex (range_end r) (range_end x)):xs)
              -- Otherwise, if the start of the range exceed the start of x, we need to remove ranges on
              -- the right, until we reach the end.
              -- This corresponds to such type of situation :
              --         [____x____] [____x1___]  [___x2___] ...
              --               [_________r___________]
              -- => rem= [__x__]                     [_x2__] ...
              --    ex =       [_r1] [____r2___]  [r3]
              | range_start r > range_start x =
                  let (ex,rem) = eat [] (range_end r) ((mk_rangex (range_start r) (range_end x)):xs)
                    in (ex,(mk_rangex (range_start x) (range_start r)):rem)
          -- In this eat, up is the upper bound of the range being removed, and acc is the list of ranges
          -- that were removed by eating.
          eat acc up [] = (acc,[])
          eat acc up l@(x:xs)
              | up >= range_end x = eat (acc ++ [x]) up xs
              -- We finished eating, and end up in the middle of a range. We shrink it by the left
              -- (dually to the second to last situation in the main extract_ function)
              | up > range_start x = (acc ++ [mk_rangex (range_start x) up],(mk_rangex up (range_end x)):xs)
              | otherwise = (acc,l)

-- | Retreive the minimum of a **non empty** range set
set_min :: RangeSet -> Int
set_min =
    set_min_ . _content
    where set_min_ (x:_) = range_start x

-- | Retrieve the maximum of a **non empty** range set
set_max :: RangeSet -> Int
set_max =
    set_max_ . _content
    where set_max_ [x] = range_end x
          set_max_ (_:xs) = set_max_ xs


-- | Compute the smallest range containing every element of the range set
set_span :: RangeSet -> Range
set_span rs = mk_rangex (set_min rs) (set_max rs)



