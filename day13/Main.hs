module Main where

import Main.Base
import Geometry.Grid
import Text.Printf
import Data.Functor
import Data.Maybe (isNothing)
import Control.Monad

type BGrid = Grid Bool
data Dir = H | V deriving (Show,Eq)

-- Return (has relfexion, need one change && changed allowed)
-- smudged = has a smudged already been considered
isLineReflection' :: (Int -> (Int,Int)) -> Int -> BGrid -> Int -> Maybe Int -> (Bool,Maybe Int)
isLineReflection' mkcoord size grid line smudged =
    aux smudged (line,line+1)
    where aux acc (l,r)
              | l < 0 || r >= size = (True,acc)
              | (grid `at` (mkcoord l) == grid `at` (mkcoord r)) = aux acc (l-1,r+1)
              | isNothing acc = aux (Just l) (l-1,r+1)
              | otherwise = (False,acc)
    

isLineReflection :: (Int -> (Int,Int)) -> Int -> BGrid -> Int -> Bool
isLineReflection mkcoord size grid line =
    aux (line,line+1)
    where aux (l,r)
              | l < 0 || r >= size = True
              | otherwise = 
                  (grid `at` (mkcoord l) == grid `at` (mkcoord r))
                  && aux (l-1,r+1)

getLineReflections :: (Int -> (Int,Int)) -> Int -> BGrid -> [Int] -> [Int]
getLineReflections mkcoord size grid candidates =
    filter (isLineReflection mkcoord size grid) candidates

getLineReflections' :: (Int -> (Int,Int)) -> Int -> BGrid -> [(Int,Maybe (Int,Int))] -> [(Int,Maybe (Int,Int))]
getLineReflections' mkcoord size grid candidates =
    map update
    $ filter (fst . snd) 
    $ map (\(l,s) -> ((l,s), isLineReflection' mkcoord size grid l (snd <$> s)))
    $ candidates
    where update ((i, Nothing),(_,Just x)) = (i, Just (i, x))
          update ((i, Nothing),(_,_)) = (i, Nothing)
          update ((i, Just xx),(_,_)) = (i, Just xx)

getReflections :: (Int -> Int -> (Int,Int)) -> BGrid -> Int -> Int -> [Int]
getReflections mkcoord grid numlines size =
    do1 [0..size-2] numlines
    where do1 acc 0 = acc
          do1 [] _ = []
          do1 acc n = do1 (getLineReflections (mkcoord $ n - 1) size grid acc) (n - 1)

getReflections' :: (Int -> Int -> (Int,Int)) -> BGrid -> Int -> Int -> [(Int,Maybe (Int,Int))]
getReflections' mkcoord grid numlines size =
    do1 (zip [0..size-2] (repeat Nothing)) numlines
    where do1 acc 0 = acc
          do1 [] _ = []
          do1 acc n = do1 (getLineReflections' (mkcoord $ n - 1) size grid acc) (n - 1)

vertical :: Int -> Int -> (Int,Int)
vertical row = (row,)

horizontal :: Int -> Int -> (Int,Int)
horizontal col = (,col)

getAllReflections :: BGrid -> [(Dir,Int)]
getAllReflections grid =
    let vrefl = getReflections vertical grid (g_height grid) (g_width grid)
        hrefl = getReflections horizontal grid (g_width grid) (g_height grid)
        in (map (H,) hrefl ++ map (V,) vrefl)

getAllReflections' :: BGrid -> [(Dir,(Int,Maybe (Int,Int)))]
getAllReflections' grid =
    let vrefl = getReflections' vertical grid (g_height grid) (g_width grid)
        hrefl = getReflections' horizontal grid (g_width grid) (g_height grid)
        in (map (H,) hrefl ++ map (V,) (map (fmap (fmap (\(x,y) -> (y,x)))) vrefl))

parse1Grid :: String -> (BGrid,String)
parse1Grid ct =
    let (gct, rem) = move [] ct in
        (parseGrid (== '#') False gct, rem)
    where move acc ('\n':'\n':xs) = (acc,xs)
          move acc [] = (acc,[])
          move acc ('\n':xs) = move (acc ++ ['\n']) xs
          move acc l = 
              let (line,rem) = span (/= '\n') l in
                move (acc ++ line) rem

printGrid :: BGrid -> IO ()
printGrid = putStrLn . (pretty (\c -> if c then '#' else '.'))

process1 :: Logger -> String -> IO ()
process1 logger ct =
    --do1 1 ct >>= \n -> logger (printf "Sum of all scores: %d" n)
    do2 1 ct >>= \(n1,n2) -> logger (printf "Sum of all scores: no smudge=%d, smudge=%d" n1 n2)
    where do1 :: Int -> String -> IO Int
          do1 _ [] = return 0
          do1 n ct =
            let (grid, ct') = parse1Grid ct in
            let refl = getAllReflections grid in 
            let result = score refl in do
                logger (printf "Grid %d (%d×%d) => %s (%d)" n (g_width grid) (g_height grid) (show refl) result)
                logger ("\n" ++ pretty (\c -> if c then '#' else '.') grid)
                next <- do1 (n + 1) ct'
                return $ result + next
          do2 :: Int -> String -> IO (Int,Int)
          do2 _ [] = return (0,0)
          do2 n ct =
            let (grid, ct') = parse1Grid ct in
            let refl = getAllReflections' grid in
            let result1 = scoreNS refl
                result2 = scoreWS refl in do
                logger (printf "Grid %d (%d×%d) =>" n (g_width grid) (g_height grid))
                forM_ refl present
                logger (printf "Score for no smudging: %d, for with smudging: %d" result1 result2)
                (nos,wis) <- do2 (n + 1) ct'
                return (nos + result1, wis + result2)
          score = sum . map score1
          score1 (H,n) = 100 * (1 + n)
          score1 (V,n) = 1 + n
          present (d,(n,m)) = present1 (show d ++ " " ++ show n) m
          present1 pre Nothing = logger pre
          present1 pre (Just x) = logger (pre ++ " by smudging " ++ show x)
          scoreNS = score . map (fmap fst) . filter (isNothing . snd . snd)
          scoreWS = score . map (fmap fst) . filter (not . isNothing . snd . snd)


main :: IO ()
main = doMain process1



