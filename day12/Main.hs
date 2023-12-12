module Main where

import Parsing
import Main.Base
import Control.Applicative
import Control.Monad
import Text.Printf
import Data.List (replicate, intercalate, group)

import Spring

parseLine :: Parser String ([Spring],[Int])
parseLine =
    (foldrP (:) [] parse1) >>= \s -> parseSpace' >> (parseNumber %:> foldrP (:) [] parse1num) >>= \ns ->
        return (s, ns)
    where parse1 = 
              (parseChar_ '.' >> return KO) <|>
              (parseChar_ '#' >> return OK) <|>
              (parseChar_ '?' >> return U)
          parse1num = parseChar_ ',' >> parseNumber

solve1 :: String -> IO ()
solve1 str =
    case execParser parseLine str of
      Nothing -> putStrLn "** Parse error **"
      Just (springs,conf) ->
        let sols = solutions0' springs conf in do
            printf "Got %d solutions:\n" (length sols)
            forM_ sols (putStrLn . show)

process1 :: Logger -> [(Int, [Spring], [Int])] -> IO ()
process1 logger confs = do
    result <- foldM proc1 0 confs
    logger ("Sum of results: " ++ show result)
    logger "Now unfolding..."
    result' <- foldM proc2 0 confs
    logger ("Sum of results: " ++ show result')
    where proc1 acc (num,springs,conf) =
              -- I calculate with both algorithms to check that they output the same thing
              -- I was very trustful of solutions0, not so much of solutions...
              let sols = solutions0 springs conf
                  sols' = solutions springs conf in do
                logger (printf " %03d - %s / %s => %d/%d solutions" 
                    num 
                    (showSprings springs) (show conf) 
                    sols sols')
                return $ acc + sols
          proc2 acc (num,springs,conf) =
              let usprings = unfoldSprings springs
                  uconf = unfoldConfs conf in
                  let sols = solutions usprings uconf in do
                      logger (printf " %03d - %s... (%d springs) / %s... (%d numbers) => %d solution"
                        num
                        (showSprings $ take 7 usprings) (length usprings)
                        (intercalate "," $ map show $ take 7 uconf) (length uconf)
                        sols)
                      return (acc + sols)
          showSprings = concat . map show

preproc1 :: Logger -> String -> IO (Either Int [(Int, [Spring], [Int])])
preproc1 logger ct =
    return $ sequence $ zipWith parse1 [1..] $ lines ct
    where parse1 i str =
              case execParser parseLine str of
                Nothing -> Left i
                Just (springs,conf) -> Right (i, springs, conf)

preerr1 :: Logger -> Either Int a -> IO ()
preerr1 logger (Left i) =
    logger (printf "Parse error at line %d" i)

main :: IO ()
main = doMainPre preproc1 preerr1 process1


