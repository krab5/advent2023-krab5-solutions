module HASHMAP.Boxes where

import Data.List (intercalate)
import qualified Data.Vector as V

data Lens = Lens { lens_id :: String, lens_focal :: Int }
-- "behind" == tail of the list, "front" = head
data Box = Box { lenses :: [Lens] }
type Boxes = V.Vector Box

instance Show Lens where
    show l = "[" ++ lens_id l ++ " " ++ show (lens_focal l) ++ "]"

instance Show Box where
    show = intercalate " " . map show . lenses

mkLens :: String -> Int -> Lens
mkLens s i = Lens { lens_id = s, lens_focal = i }

lens_power :: Int -> Int -> Lens -> Int
lens_power boxnum pos lens = boxnum * pos * lens_focal lens

box_power :: Int -> Box -> Int
box_power boxnum =
    sum . zipWith (lens_power $ 1 + boxnum) [1..] . lenses

boxes_power :: Boxes -> Int
boxes_power =
    V.sum . V.imap box_power
    

isEmpty :: Box -> Bool
isEmpty = null . lenses

removeLens :: String -> Box -> Box
removeLens l b = 
    b { lenses = rem (lenses b) }
    where rem [] = []
          rem (x:xs)
              | l == lens_id x = xs
              | otherwise = x:(rem xs)

addLens :: Lens -> Box -> Box
addLens l b =
    b { lenses = add (lenses b) }
    where add [] = [l]
          add (x:xs)
              | lens_id l == lens_id x = l:xs
              | otherwise = x:(add xs)

setBox :: Boxes -> Int -> (Box -> Box) -> Boxes
setBox boxes n f =
    boxes V.// [(n, f (boxes V.! n))]

emptyBox :: Box
emptyBox = Box { lenses = [] }

emptyBoxes :: Boxes
emptyBoxes = V.replicate 256 emptyBox

pretty :: Boxes -> String
pretty =
    V.ifoldl show1 ""
    where show1 acc n b
              | isEmpty b = acc
              | otherwise = acc ++ "Box " ++ show n ++ ": " ++ show b ++ "\n"




