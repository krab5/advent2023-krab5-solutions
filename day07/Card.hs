module Card where

import Data.List (sortOn, groupBy)
import Data.Ord
import Data.Function
import Control.Monad
import qualified Data.Char as Char
import Text.Printf

data Value = Value { _ord :: Int } deriving (Eq,Ord)

mkValue :: Int -> Value
mkValue n | 1 <= n && n <= 14 = Value { _ord = n }

joker :: Value
joker = mkValue 1

jAsJoker :: Value -> Value
jAsJoker v
    | _ord v == 11 = joker
    | otherwise = v

instance Show Value where
  show (Value n) =
      [rep !! n]
      where rep = "?*23456789TJQKA"

readC :: Char -> Value
readC 'A' = mkValue 14
readC 'K' = mkValue 13
readC 'Q' = mkValue 12
readC 'J' = mkValue 11
readC 'T' = mkValue 10
readC x | Char.isDigit x = mkValue $ Char.digitToInt x

readC_joker :: Char -> Value
readC_joker 'J' = mkValue 1
readC_joker x = readC x

data Card = Card { _value :: Value, _pos :: Int } deriving Eq

isJoker :: Card -> Bool
isJoker = (== joker) . _value

asJoker :: Card -> Card
asJoker c = c { _value = jAsJoker $ _value c }

instance Show Card where
  show c = printf "<%s>@%d" (show $ _value c) (_pos c)

instance Ord Card where
  compare (Card v p) (Card v' p') =
      (compare v v') <> (compare p p')

insertSortD :: Ord a => a -> [a] -> [a]
insertSortD e [] = [e]
insertSortD e (x:xs)
    | e < x = x:(insertSortD e xs)
    | otherwise = e:x:xs

data Figure = High | OnePair | TwoPair | Three | FullHouse | Four | Five deriving (Eq,Ord,Enum,Show)

-- Figures with jokers: 
-- (1) XXXX*, XXX**, XX***, X****, ***** => Five
-- (2) AXXX*, AXX**, AX*** => Four
-- (3) ABXX*, ABX** => Three
-- (4) ABCX* => OnePair
-- (5) AAXX* => FullHouse (not AAX** which is a four of A)
--
-- There cannot be two pairs: a two pair config is e.g. AAB*C which is better as a three of A

calcFigure :: [Card] -> Figure
calcFigure cs =
    let (normal,jokers) = span (not . isJoker) cs in
        let groups = sortOn Down $ map length $ groupBy ((==) `on` _value) normal in
            case (groups, length jokers) of
              ([],5) -> Five
              ((n:_),n') | n + n' == 5 -> Five -- one type of card + n jokers => case 1
              ((n:_),n') | n + n' == 4 -> Four -- one lone card + one other type + n jokers => case 2
              ([n,2],n') | n + n' == 3 -> FullHouse -- two types of cards + 1 joker = case 5
              ((n:_),n') | n + n' == 3 -> Three -- three types of cards + n jokers = case 3
              ([2,2,1],0) -> TwoPair
              ([n,_,_,_],n') | n + n' == 2 -> OnePair -- four types of cards + 1 joker = case 4
              _ -> High

data Hand = Hand { _cards :: [Card], getFigure :: Figure } deriving (Eq)

instance Show Hand where
  show h = concat $ map (show . _value . nth h) $ [0..size h - 1]

mkHand :: [Card] -> Hand
mkHand cs = Hand { _cards = cs, getFigure = calcFigure cs }

nth :: Hand -> Int -> Card
nth h n =
    nth_ $ _cards h
    where nth_ (x:xs)
            | _pos x == n = x
            | otherwise = nth_ xs

size :: Hand -> Int
size = length . _cards

readHand :: String -> Hand
readHand =
    mkHand . foldr mkAndInsert [] . zip [0..]
    where mkAndInsert (n,c) = insertSortD (Card { _value = readC c, _pos = n })

readHand' :: String -> Hand
readHand' =
    mkHand . foldr mkAndInsert [] . zip [0..]
    where mkAndInsert (n,c) = insertSortD (Card { _value = readC_joker c, _pos = n })

cards :: Hand -> [Card]
cards h = map (nth h) [0..(size h - 1)]

instance Ord Hand where
  compare h h' =
      compare (getFigure h) (getFigure h') <> compare (cards h) (cards h')




