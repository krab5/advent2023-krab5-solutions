module Main where

import Parsing
import Main.Base
import Data.Maybe (mapMaybe)
import Text.Printf

sortInsert :: Ord a => a -> [a] -> [a]
sortInsert x [] = [x]
sortInsert x (y:ys)
    | y > x = x:y:ys
    | otherwise = y:(sortInsert x ys)

intersect :: Ord a => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect (x:xs) (y:ys)
    | x == y    = x:(intersect xs ys)
    | x > y     = intersect (x:xs) ys
    | otherwise = intersect xs (y:ys) 

data Card = Card {
    card_id :: Int,
    card_winning :: [Int],
    card_obtained :: [Int],
    card_copies :: Int
} deriving Show

inc_copies :: Card -> Card
inc_copies c = c { card_copies = (card_copies c) + 1}

add_copies :: Card -> Int -> Card
add_copies c i = c { card_copies = (card_copies c) + i }

parseCard :: Parser [Char] Card
parseCard =
    parseSeq_ "Card" >> parseSpaces >> parseNumber >>= \i -> parseChar_ ':' >>
        (foldrP sortInsert [] (parseSpaces >> parseNumber)) >>= \w -> parseSpaces >> parseChar_ '|' >>
            (foldrP sortInsert [] (parseSpaces >> parseNumber)) >>= \o ->
                return (Card { card_id = i, card_winning = w, card_obtained = o, card_copies = 1 })

num_win :: Card -> Int
num_win c =
    length i
    where w = card_winning c
          o = card_obtained c
          i = intersect w o

score :: Card -> Int
score c =
    let l = num_win c in
        if l == 0 then 0 else 2 ^ (l - 1)

scratch :: [Card] -> [Card]
scratch [] = []
scratch (c:cs) =
    let l = num_win c in
        c:(scratch (zipWith add_copies cs (mask l (card_copies c))))
    where mask :: Int -> Int -> [Int]
          mask n k = (replicate n k) ++ (repeat 0)

score' :: [Card] -> Int
score' = sum . map card_copies

process1 :: Logger -> String -> IO ()
process1 logger ct =
    let cards = mapMaybe (execParser parseCard) $ lines ct in
        let scores = map score cards in do
            logger (printf "Parsed %d cards" (length cards))
            logger (printf "Total score: %d" (sum scores))
            let scratched = scratch cards in
                logger (printf "Total number of cards: %d" (score' scratched))

main :: IO ()
main = doMain process1




