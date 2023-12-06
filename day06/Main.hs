module Main where

import Main.Base
import Parsing
import Control.Applicative
import Control.Monad
import Text.Printf
import Data.Bifunctor

parse :: Parser [Char] ([Int],[Int])
parse =
    parseSeq_ "Time:" >> foldrP (:) [] (parseSpaces' >> parseNumber) >>= \times -> parseSpaces0 >>
        parseSeq_ "Distance:" >> foldrP (:) [] (parseSpaces' >> parseNumber) >>= \dist -> parseSpaces0 >>
            end >> return (times, dist)

solve :: Int -> Int -> [Int]
solve maxTime record = do
    pushTime <- [0..maxTime]
    score <- return (pushTime * (maxTime - pushTime))
    guard (score > record)
    return score

concatNums :: [Int] -> Int
concatNums =
    snd . concat_
    where concat_ [] = (1,0)
          concat_ (x:xs) =
              let (nd,r) = concat_ xs in
                  (nd * 10 * rank x, x * nd + r)
          rank x = if x < 10 then 1 else 10 * (rank (x `div` 10))

process1 :: Logger -> String -> IO ()
process1 logger ct =
    case execParser parse ct of
      Nothing -> logger "Parse error"
      Just parsed ->
          let races = uncurry zip parsed in
              let results = map (uncurry solve) races in do
                  logger (printf "Found %d races configurations" (length races))
                  result <- foldM proc1 1 (zip races results)
                  logger (printf "Product of #solutions: %d" result)
                  let (rtime,rdist) = bimap concatNums concatNums parsed
                      result' = solve rtime rdist in do
                      logger (printf "Actual race time=%d, actual distance=%d" rtime rdist)
                      logger (printf "Number of optimal solutions: %d" (length result'))
    where proc1 acc ((t,d),rs) = do
              logger (printf " - Time=%d, Record=%d => %d better solutions" t d (length rs))
              return (acc * (length rs))


main :: IO ()
main = doMain process1


