module Spring where

import Control.Parallel
import Data.List (intercalate)

-- A spring : U is unknown, OK is a normal spring and KO is broken
data Spring = U | OK | KO deriving (Eq,Ord)

instance Show Spring where
    show U = "?"
    show OK = "#"
    show KO = "."

infix 7 `fastMult`

-- fastMult exploits lazy evaluation to avoid avaluating the rhs when the lhs is 0
-- In the algorithms here this is genuinely faster because a few configurations generate
-- something like solutions [] [...] which yields 0
fastMult :: Integer -> Integer -> Integer
fastMult 0 _ = 0
fastMult _ 0 = 0
fastMult n m = n * m

-- Count the possibilities for having req contiguous OK in a list of spring
atomic :: [Spring] -> Int -> Integer
atomic [] 0 = 1   -- 0 OK in 0 springs = 1 solution
atomic [] _ = 0   -- any number of OK in 0 springs = 0 solutions
atomic l@(x:xs) req
    -- we need precisely req springs so if the length is not sufficient, do not check
    | length l >= req = 
        case x of
            -- KO are ignored, only serving as delimiters in some way
            KO -> atomic xs req
            -- If we start with an OK, then that spring *must* be part of the solution
            -- This means that, if we can have req springs that are OK or U and after that
            -- no more OK (i.e. everything can be reduced to KO), then this is a solution,
            -- otherwise, not.
            -- In addition, no need to check further; there cannot be any other solution leaving
            -- out the OK.
            OK ->
                let (taken,rem) = splitAt req l in
                    if all (/= KO) taken && all (/= OK) rem then 1 else 0
            -- If this is a U, we try to take req springs and check if each of them is OK or U, then check
            -- that there is no OK in the remainder (any OK *must* be part of *every* solutions).
            -- We then offset the window and continue searching.
            U ->
                let (taken,rem) = splitAt req l in
                    (if all (/= KO) taken && all (/= OK) rem then 1 else 0)
                    + atomic xs req
    | otherwise = 0

-- Break appart the list of springs according to the given pivot.
-- This searches for any sequence of piv symbols that can be contiguous set of OK springs,
-- and return the list of springs on the left of that and the one on the right, both
-- correctly shielded (i.e., if piv = 4 and I find ---?####?---, I need to make sure both
-- the jokers are in fact KO, otherwise the stream of springs is not actually 4).
--
-- The function also takes as input the minimum number of pieces required on the left and 
-- on the right, to prune irrelevant configurations early on.
--
-- Typically, if you require at least 3 pieces on the left, you will exclude partitions such
-- as ([],[...]), ([?],[...]), ([??],[...]) and so on.
pivot :: [Spring] -> Int -> Int -> Int -> [([Spring],[Spring])]
pivot springs lsize rsize piv 
    | length springs < piv = []
    | otherwise =
        let (window,rem) = splitAt piv springs
            in pivot_ 0 (length springs - piv) window [] rem
        -- The pivot searches for a stream of valid springs (a window), that is offset
        -- progressively until we reach the end
        where pivot_ ls rs _ [] [] = [] -- no more movement
              pivot_ ls rs (w:ws) acc (s:ss)
                  -- Move the window until there are enough elements on the left
                  | ls < lsize = pivot_ (ls + 1) (rs - 1) (ws ++ [s]) (w:acc) (ss)
                  -- If there are not enough elements on the right, no need to go further
                  | rs < rsize = []
              -- Special case for the beginning (unlikely to be called with the heuristic
              pivot_ ls rs window@(w:ws) [] springs@(next:sprs) = 
                  let this = if possible KO window next then [([],KO:sprs)] else []
                      in this ++ pivot_ (ls + 1) (rs - 1) (ws ++ [next]) [w] (sprs)
              -- Special case for the end
              pivot_ ls rs window acc@(prev:accs) [] = 
                  if possible prev window KO then [(reverse (KO:accs), [])] else []
              -- Determine if the current window may be used as a pivot (using possible)
              -- Offset the window to the right
              pivot_ ls rs window@(w:ws) acc@(prev:accs) springs@(next:sprs) =
                  let this = if possible prev window next then [(reverse (KO:accs), KO:sprs)] else []
                      in this ++ pivot_ (ls + 1) (rs - 1) (ws ++ [next]) (w:acc) (sprs)
              -- The window may be used as a pivot if it is composed of OK or U, and if it can be 
              -- shileded, i.e. surrounded by KO (any spring that could be OK otherwise will
              -- make the window larger, and thus invalid w.r.t to the given size)
              possible prev window next = (prev /= OK ) && (next /= OK) && all (/= KO) window

-- Find a good pivot, return the elements on the left and on the right of the pivot,
-- in order.
-- Here, the chosen pivot is the first maximum. This is fine when the input is small
-- enough, but tend to produce unbalanced configurations.
good_pivot :: [Int] -> ([Int],Int,[Int])
good_pivot (l:ls) = 
    aux [] l [] ls
    where aux left p acc [] = (reverse left, p, reverse acc)
          aux left p acc (x:xs)
              | p < x = aux (acc ++ p:left) x [] xs
              | otherwise = aux left p (x:acc) xs

-- Find a good pivot.
-- This version uses the middle of the list. This produces balanced configurations but this
-- is not very good; in general we want to use the maximum in the pivot, because it prunes so
-- much cases all at once (consider the possibilities of ?????? 4 vs ?????? 1 for example).
good_pivot' :: [Int] -> ([Int],Int,[Int])
good_pivot' [x] = ([],x,[])
good_pivot' [x,y] = ([x],y,[])
good_pivot' l =
    let (left,x:right) = splitAt (length l `div` 2) l
        in (left, x, right)

-- Find a good pivot.
-- This version uses good_pivot but tries to balance the left and right by finding next maximums
-- until the size of left is greather than that of right.
-- This heuristic is very good, and tones down some calculations greatly. This is especially relevant
-- when maximums are scattered equiprobably among the input, which is the case in the challenge since
-- we repeat the same cycle (i.e., [1,1,7] becomes [1,1,7,1,1,7,1,1,7,1,1,7,1,1,7]).
good_pivot'' :: [Int] -> ([Int],Int,[Int])
good_pivot'' ls =
    let (left,p,right) = good_pivot ls in
        offset left p right
    where offset left p right
              | length left < length right && p `elem` right =
                  let (rr,_:rs) = span (/= p) right in
                    offset (left ++ p:rr) p rs
              | otherwise = (left,p,right)

-- Find the solutions with a divide-and-conquer strategy.
-- First, calculate a pivot, with left and right configurations
-- Second, find every way to split the springs in two by extracting a correct stream of springs which
-- size is the given pivot
-- Third, calculate solutions for the LHS of the springs with the LHS of the pivot, and similarly for the RHS,
-- this for each pivot configuration
-- Fourth, multiply both result (for each pivot) and sum it all, and that's it!
-- If the configuration is of size 1, perform an _atomic_ calculation (see above).
-- If the configuration is of size 0, just check that there are no OK pieces (everything may be reduced to KO)
solutions :: [Spring] -> [Int] -> Integer
solutions [] [] = 1
solutions springs [] 
    | not (any (== OK) springs) = 1
    | otherwise = 0
solutions [] _ = 0
solutions springs [x] = atomic springs x
solutions springs conf =
    let pivoted = pivot springs (sum gleft) (sum gright) gpiv in
        sum $ map doOne pivoted
    where (gleft,gpiv,gright) = good_pivot'' conf
          doOne (left,right) =
              let soll = solutions left gleft
                  solr = solutions right gright
                  in soll `fastMult` solr

-- PRIOR TENTATIVE (not efficient enough)

-- Split at a given point while the given predicate is true.
-- This returns Just (xs,ys) with xs of size n and forall x in xs, p x, if possible
-- Otherwise, it returns Nothing
splitAtWhile :: (a -> Bool) -> Int -> [a] -> Maybe ([a],[a])
splitAtWhile p n x =
    aux [] n x
    where aux acc 0 rs = Just (reverse acc, rs)
          aux acc n [] = Nothing -- n != 0
          aux acc n (x:xs)
              | p x = aux (x:acc) (n - 1) xs
              | otherwise = Nothing

-- Calculate the solutions by trying for every combinations possible
-- This algorithm was doomed to fail because it only performs additions...
-- Note however that it does generate the correct solutions!
solutions0 :: [Spring] -> [Int] -> Integer
solutions0 [] [] = 1 -- no spring left, no config left = ok
solutions0 [] _  = 0   -- no spring left, config left = ko (no solution)
solutions0 springs []
    | all (/= OK) springs = 1 -- no config (no more ok spring) => must all be broken (and if unknown then => broken)
    | otherwise = 0 -- there is an ok spring but no more config = no solution
solutions0 springs@(s:ss) config@(n:ns) = 
    this1 `par` next1 `par` (this1 + next1)
    where next1 = if s == OK then 0 else solutions0 ss config
          this1 =
            case splitAtWhile (/= KO) n springs of
                Nothing -> 0
                Just (taken,[]) -> solutions0 [] ns 
                Just (taken,OK:rs) -> 0
                Just (taken,r:rs) -> solutions0 rs ns

-- Same as above but actually generate the solutions, which is very convenient for debugging
solutions0' :: [Spring] -> [Int] -> [[Spring]]
solutions0' [] [] = [[]] -- no spring left, no config left = ok
solutions0' [] _  = []   -- no spring left, config left = ko (no solution)
solutions0' springs []
    | all (/= OK) springs = [map (const KO) springs] -- no config (no more ok spring) => must all be broken (and if unknown then => broken)
    | otherwise = [] -- there is an ok spring but no more config = no solution
solutions0' springs@(s:ss) config@(n:ns) = 
    this1 ++ next1
    where next1 = if s == OK then [] else solutions0' ss config >>= (return . (KO:))
          this1 =
            case splitAtWhile (/= KO) n springs of
                Nothing -> []
                Just (taken,[]) -> solutions0' [] ns >>= (return . (map (const OK) taken ++))
                Just (taken,OK:rs) -> []
                Just (taken,r:rs) -> 
                    let t = (map (const OK) taken) in solutions0' rs ns >>= (return . (t ++) . (KO:))
 

-- Unfold springs, as detailed in the challenge
unfoldSprings :: [Spring] -> [Spring]
unfoldSprings = intercalate [U] . replicate 5

-- Unfold configuration, as detailed in the challenge
unfoldConfs :: [Int] -> [Int]
unfoldConfs = concat . replicate 5



