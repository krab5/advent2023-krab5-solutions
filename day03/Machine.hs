module Machine where

import Geometry.Rect
import Data.Char (isDigit)

data Part = Part { 
    part_id :: Int, 
    part_rect :: Rect Int
} deriving Show

data Symbol = Symbol { 
    symbol :: Char,
    symbol_pos :: (Int,Int)
} deriving Show

data Machine = Machine {
    symbols :: [Symbol],
    parts :: [Part],
    area :: Rect Int
} deriving Show

parseLine :: Int -> String -> ([Part],[Symbol])
parseLine posy line =
    eat iline
    where iline = zip line [0..]
          eat [] = ([],[])
          eat l@(x@(c,i):xs)
            | isDigit c = 
                let (num,rem) = span (isDigit . fst) l in
                    let (ps,ss) = eat rem in
                        combinePart num (ps,ss)
            | c == '.' = eat (dropWhile ((== '.') . fst) l)
            | otherwise =
                let (ps,ss) = eat xs in
                    combineSymbol x (ps,ss)
          combinePart num (ps,ss) =
              let number = read $ map fst $ num
                  posx = snd $ head num
                  posx' = snd $ last num in
                  let p = Part { part_id = number, part_rect = mkRect (posx, posy) (posx' - posx, 0) }
                    in (p:ps,ss)
          combineSymbol (c,i) (ps,ss) =
              let s = Symbol { symbol = c, symbol_pos = (i,posy) } in
                  (ps,s:ss)


parseGrid :: String -> Machine
parseGrid ct =
    let ls = lines ct in
        let (ps,ss) = foldl combine ([],[]) $ zipWith parseLine [0..] $ lines ct
          in Machine { symbols = ss, parts = ps, area = mkRect (0,0) (length (head ls), length ls) }
    where combine (ps,ss) (ps',ss') = (ps ++ ps', ss ++ ss')

surroundings :: (Int, Int) -> Rect Int
surroundings (x,y) = mkRect (x - 1, y - 1) (2, 2)

partOk :: [Symbol] -> Part -> Bool
partOk ss p =
    any (intersect (part_rect p) . surroundings . symbol_pos) ss

isGear :: [Part] -> Symbol -> Maybe Int
isGear ps s =
    if (symbol s == '*') && (length parts == 2)
        then let (g1:g2:_) = parts in Just $ part_id g1 * part_id g2
        else Nothing
    where parts = filter (intersect (surroundings $ symbol_pos s) . part_rect) ps





