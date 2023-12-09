module Main where

import Main.Base
import Parsing
import Control.Monad
import Text.Printf

next :: [Int] -> Int
next [] = 0
next l@(x:xs) =
    let r = next (zipWith (-) l (tail l)) in
        r + x

parseNumSeq :: Parser String [Int]
parseNumSeq =
    parseNumber %:> foldrP (:) [] (parseSpace' >> parseNumber)


process1 :: Logger -> String -> IO ()
process1 logger ct =
    let res = map (execParser parseNumSeq) $ lines ct in do
        (resnext, resprev) <- foldM proc1 (0, 0) res
        logger (printf "Sum of next: %d, sum of prev: %d" resnext resprev)
    where proc1 (ne,pr) Nothing = logger "Parse error" >> return (ne,pr)
          proc1 (ne,pr) (Just seq) =
              let theNext = next (reverse seq) 
                  thePrev = next seq in do
                  logger (printf "%s => next: %d, previous: %d" (show seq) theNext thePrev)
                  return (ne + theNext, pr + thePrev)

main :: IO ()
main = doMain process1


