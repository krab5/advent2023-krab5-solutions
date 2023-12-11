module Main where

import Main.Base
import Text.Printf
import Control.Monad

parseLine :: Int -> Int -> [Int] -> String -> ([(Int,Int)],[Int])
parseLine _ _ _ [] = ([],[])
--parseLine _ _ [] _ = ([],[]) -- should not happen
parseLine row col ns (x:xs)
    | x == '#' =
        let galaxy = (row,col)
            ns' = takeIfEq col ns
            in let (galaxies,ns'') = parseLine row (col + 1) ns' xs
                in (galaxy:galaxies, ns'')
    | otherwise =
        let ns' = takeIfEq col ns
            in let (galaxies, ns'') = parseLine row (col + 1) ns' xs
                in (galaxies, putBackIfEq col ns'' ns)
    where takeIfEq _ [] = []
          takeIfEq c ns@(n:nss) = if c == n then nss else ns
          putBackIfEq c ns' [] = ns'
          putBackIfEq c ns' (n:_) = if c == n then n:ns' else ns'

parseGrid :: String -> ([(Int,Int)],[Int],[Int])
parseGrid str =
    let ls@(l:_) = lines str in
        parse1 0 [0..length l - 1] ls
    where parse1 _ cols [] = ([],cols,[])
          parse1 row cols (l:ls) =
            let (galaxies,cols') = parseLine row 0 cols l in
                let (galaxies',cols'',rows) = parse1 (row + 1) cols' ls in
                    (galaxies ++ galaxies', cols'', if null galaxies then row:rows else rows)

expand1 :: Integer -> [Int] -> [Int] -> (Int,Int) -> (Integer,Integer)
expand1 factor rows cols (r,c) =
    let rbefore = takeWhile (< r) rows
        cbefore = takeWhile (< c) cols
        in ( fromIntegral r + factor * fromIntegral (length rbefore)
           , fromIntegral c + factor * fromIntegral (length cbefore))

expand :: Integer -> [Int] -> [Int] -> [(Int,Int)] -> [(Integer,Integer)]
expand factor rows cols = map (expand1 (factor - 1) rows cols)

manhattan :: (Num a, Ord a) => (a,a) -> (a,a) -> a
manhattan (x1,y1) (x2,y2) =
    (if x1 > x2 then x1 - x2 else x2 - x1) + (if y1 > y2 then y1 - y2 else y2 - y1)

distances :: (Num a, Ord a) => [(a,a)] -> a
distances [] = 0
distances (x:xs) =
    foldl (\acc x' -> acc + manhattan x x') (distances xs) xs

process1 :: Logger -> String -> IO ()
process1 logger ct = do
    logger (printf "Got %d galaxies" (length galaxies))
    logger ("Empty rows: " ++ show erows)
    logger ("Empty columns: " ++ show ecols)
    logger (printf "Total Manhattan distance between galaxies: %d" (distances galaxies))
    let galaxies1 = expand 2 erows ecols galaxies
    logger (printf "Total Manhattan distance between galaxies after expansion: %d" (distances galaxies1))
    let galaxies10 = expand 10 erows ecols galaxies
    logger (printf "Total Manhattan distance between galaxies after expansion times 10: %d" (distances galaxies10))
    let galaxies1m = expand 1000000 erows ecols galaxies
    logger (printf "Total Manhattan distance between galaxies after expansion times 1000000: %d" (distances galaxies1m))
    where (galaxies,ecols,erows) = parseGrid ct

main :: IO ()
main = doMain process1


