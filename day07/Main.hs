module Main where

import Main.Base
import Parsing
import Data.List (sort, sortOn)
import Data.Ord
import Data.Function
import Control.Monad
import qualified Data.Char as Char
import Text.Printf

import Card

parseLine :: Parser String (Hand,Hand,Int)
parseLine =
    (parseP Char.isAlphaNum `ptimes` 5) >>= \h -> parseSpaces >> parseNumber >>= \n -> parseSpaces0 >> end >>
        return (readHand h, readHand' h, n)

preproc1 :: Logger -> String -> IO (Maybe [(Hand,Hand,Int)])
preproc1 logger ct = return $ sequence $ map (execParser parseLine) $ lines ct

process1 :: Logger -> [(Hand,Hand,Int)] -> IO ()
process1 logger hands =
    let sorted  = sort $ map (\(x,_,z) -> (x,z)) hands
        sorted' = sort $ map (\(_,y,z) -> (y,z)) hands in do
        logger (printf "Parsed %d hands" (length sorted))
        do1 sorted
        logger "After J => *"
        do1 sorted'
    where do1 :: [(Hand,Int)] -> IO ()
          do1 sorted = do
              result <- foldM proc1 0 (zip sorted [1..])
              logger (printf "Total score: %d" result)
          proc1 :: Int -> ((Hand,Int),Int) -> IO Int
          proc1 acc ((hand,bid),rank) = 
              let score = bid * rank in do
                  logger (printf "(%2d) %s [%s] => %d" rank (show hand) (show $ getFigure hand) score)
                  return $ acc + score

main :: IO ()
main = doMainPre preproc1 (\logger _ -> logger "Parse error") process1


