module Main where

import Main.Base
import Text.Printf
import qualified Data.Char as Char
import Data.Maybe (fromMaybe, fromJust)
import Control.Applicative
import Control.Monad
import Parsing

parseNumLetter :: Parser [Char] Char
parseNumLetter =
    asum [
        mkParser "one"   '1',
        mkParser "two"   '2',
        mkParser "three" '3',
        mkParser "four"  '4',
        mkParser "five"  '5',
        mkParser "six"   '6',
        mkParser "seven" '7',
        mkParser "eight" '8',
        mkParser "nine"  '9',
        mkParser "zero"  '0'
    ]
    where mkParser :: [Char] -> Char -> Parser [Char] Char
          mkParser seq ret = parseAndBack (parseSeq_ seq) >> drop1 >> return ret

processString :: Parser [Char] [Char]
processString = (end >> return []) <|> ((parseNumLetter <|> keep1) %:> processString)

readInt :: String -> Integer
readInt [] = 0
readInt (x:xs) = 
    read (x:readInt1 (x:xs)) :: Integer
    where readInt1 [x] = [x]
          readInt1 (_:xs) = readInt1 xs

parseNums :: String -> String
parseNums s = fromMaybe s (snd <$> runParser processString s)

process1 :: Logger -> String -> IO ()
process1 logger ct =
    let theLines = lines ct in
        let result1 = sum $ map (readInt . (filter Char.isDigit)) $ theLines 
            result2 = sum $ map (readInt . (filter Char.isDigit) . parseNums) $ theLines
          in do logger (printf "Ingested %d lines" (length theLines))
                logger (printf "Result: %d" result1)
                logger (printf "Result (parsing hidden digits): %d" result2)

main :: IO ()
main = doMain process1


