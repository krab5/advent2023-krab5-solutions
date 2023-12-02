module Main where

import Main.Base
import Parsing
import Control.Applicative
import Control.Monad
import Text.Printf

data Color = Red | Blue | Green deriving (Enum,Eq,Ord,Show)
data Draw = Draw { nred :: Int, nblue :: Int, ngreen :: Int } deriving Show
data Game = Game { game_id :: Int, draws :: [Draw] } deriving (Show)

setred :: Int -> Draw -> Draw
setred n c = c { nred = n }

setblue :: Int -> Draw -> Draw
setblue n c = c { nblue = n }

setgreen :: Int -> Draw -> Draw
setgreen n c = c { ngreen = n }

initc :: Draw
initc = Draw 0 0 0


parseColor :: Parser [Char] (Int -> Draw -> Draw)
parseColor =
    (parseSeq_ "red" >> return setred) 
    <|> (parseSeq_ "blue" >> return setblue) 
    <|> (parseSeq_ "green" >> return setgreen)

parseCubes :: Parser [Char] (Draw -> Draw)
parseCubes =
    parseNumber >>= \n -> parseSpaces >> parseColor >>= \c -> return (c $ n)

parseDraw :: Parser [Char] Draw
parseDraw =
    parseCubes >>= \set -> parseDraw0 >>= \c -> return (set c)
    where parseDraw0 = 
            ((parseChar_ ',' >> parseSpaces) >> parseCubes >>= \set -> parseDraw0 >>= \c -> return (set c)) 
            <|> (return initc)

parseGame :: Parser [Char] Game
parseGame =
    parseSeq_ "Game" >> parseSpaces >> parseNumber >>= \n -> parseChar_ ':' >> parseSpaces >>
        (parseDraw %:> parseDraws0) >>= \ds -> end >> return (Game n ds)
    where parseDraws0 = ((parseChar_ ';' >> parseSpaces) >> parseDraw %:> parseDraws0) <|> zero

getGames :: String -> [Game]
getGames ct = 
    case sequence $ map (runParser parseGame) $ lines ct of
      Nothing -> []
      Just x -> map snd x

getConfig :: Logger -> IO Draw
getConfig logger = do
    logger "Enter configuration:"
    l <- getLine
    case runParser parseDraw l of
      Nothing -> logger "Parser error" >> getConfig logger
      Just (_, d) -> return d

isDrawPossible :: Draw -> Draw -> Bool
isDrawPossible config draw =
    (nred config >= nred draw) && (nblue config >= nblue draw) && (ngreen config >= ngreen draw)

isGamePossible :: Draw -> Game -> Bool
isGamePossible config = all (isDrawPossible config) . draws


power :: Game -> Int
power game = 
    (nred maxG) * (nblue maxG) * (ngreen maxG)
    where maxD d1 d2 = Draw { nred = max (nred d1) (nred d2)
                            , ngreen = max (ngreen d1) (ngreen d2)
                            , nblue = max (nblue d1) (nblue d2) }
          maxG = foldl maxD initc (draws game)

process1 :: Logger -> String -> IO ()
process1 logger ct =
    let gs = getGames ct in do
        logger (printf "Parsed %d games" (length gs))
        d <- getConfig logger
        let f = map game_id $ filter (isGamePossible d) gs in do
            logger (printf "Possible games: %s" (show f))
            logger (printf "Sum: %d" (sum f))
        let p = sum $ map power gs in do
            logger (printf "Total power: %d" p)

main :: IO ()
main = doMain process1


