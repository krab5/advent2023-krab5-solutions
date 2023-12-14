{-|
Module: Grid
Description: Grid system (immutable)
Copyright: (c) krab5, 2023
License: GPL-3
Maintainer: crab.delicieux@gmail.com

This small module implements a grid system with default value (for edge cases) and
with an underlying boxed efficient vector.

It almost looks like a wrapper for vector with a coordinate transformation function,
plus some custom, practical functions on it.

Mutable version: see `Geometry.Grid.Mutable`.

-}
module Geometry.Grid where

import Data.List (intercalate)
import Data.Maybe (fromMaybe, mapMaybe)
import Control.Monad ((>>=))
import qualified Data.Vector as V

-- | The grid type.
data Grid a = Grid {
    g_default :: a,             -- ^ Default value; used for safe indexation (returned when coordinate out of bounds)
    g_width :: Int,             -- ^ Grid's width
    g_height :: Int,            -- ^ Grid's height
    g_content :: V.Vector a     -- ^ Grid's content (should _never_ be accessed directly!). This is a 1-dimensional vector; values are stored by by line.
}

instance Eq a => Eq (Grid a) where
    g1 == g2 = 
        (g_width g1 == g_width g2) && (g_height g1 == g_height g2) 
        && (g_default g1 == g_default g2)
        && V.eqBy (==) (g_content g1) (g_content g2)

-- | Test if the given coordinates are in the boundaries set by the grid.
inBound :: Grid a -> (Int,Int) -> Bool
inBound g (row,column) =
    0 <= row && row < g_height g && 0 <= column && column < g_width g

-- | Internal function that calculates the index associated to the given coordinates. 
-- The index is `Nothing` when it is out of bound.
_id :: Grid a -> (Int,Int) -> Maybe Int
_id g (row,column) 
    | inBound g (row,column) = Just $ column + row * g_width g
    | otherwise = Nothing

-- | Calculate the coordinates associated to an index. Mostly useful for find-type functions.
_coord :: Grid a -> Int -> (Int,Int)
_coord g i =
    i `divMod` (g_width g)

-- | Get the value in the grid at the given coordinates. If the coordinates are out of bound, return
-- the provided default value.
at :: Grid a -> (Int,Int) -> a
at g c = fromMaybe (g_default g) (_id g c >>= (g_content g V.!?))

-- | Set the value in the grid at the given coordinates. If the coordinates are out of bound, does
-- nothing.
set :: Grid a -> (Int,Int) -> a -> Grid a
set g c v = 
    case _id g c of
        Nothing -> g
        Just i -> g { g_content = g_content g V.// [(i, v)] }

-- | Sets a batch of values as a list of (coordinate, value) pairs. Coordinates out of bound are
-- ignored.
sets :: Grid a -> [((Int,Int), a)] -> Grid a
sets g vs =
    let ss = mapMaybe (\(x,v) -> (,v) <$> _id g x) $ vs
        in g { g_content = V.unsafeUpd (g_content g) ss }

-- | Get the list of elements in the given row (in order).
getRow :: Grid a -> Int -> [a]
getRow g r = map (\c -> g `at` (r,c)) [0..g_width g - 1]

-- | Get the list of elements in the given column (in order).
getColumn :: Grid a -> Int -> [a]
getColumn g c = map (\r -> g `at` (r,c)) [0..g_height g - 1]

-- | Do a left fold on the grid, with the combining function also presenting the coordinates.
-- Elements are presented column-wise then row-wise, i.e. (0,0),(0,1),(0,2)...(1,0),(1,1),...
foldlc :: (b -> (Int,Int) -> a -> b) -> b -> Grid a -> b
foldlc f z g =
    V.ifoldl (\acc i x -> f acc (_coord g i) x) z $ g_content g

-- | Find the coordinates of an element in the grid that satisfies the given predicate.
-- Which element is returned first is uncertain in theory (in practice, it will probably be
-- the smallest in term of (row,column), in lexicographic order).
findCoord :: (a -> Bool) -> Grid a -> Maybe (Int,Int)
findCoord p g =
    _coord g <$> V.findIndex p (g_content g)

-- | Get the list of every valid coordinates in the grid.
allCoords :: Grid a -> [(Int,Int)]
allCoords grid =
    [ (i, j) | i <- [0..g_width grid - 1]
             , j <- [0..g_height grid - 1] ]

-- | Transform a string into a grid using the provided character parsing function.
parseGrid :: (Char -> a) -> a -> String -> Grid a
parseGrid parseC defval ct =
    let ls@(l:_) = lines ct in
        Grid { g_width = length l
             , g_height = length ls
             , g_default = defval
             , g_content = V.fromList $ map parseC $ concat ls }

-- | Transform a grid into a string for pretty printing, using the provided type-to-character function.
pretty :: (a -> Char) -> Grid a -> String
pretty showC g =
    intercalate "\n" $ map (\ro -> map (\co -> showC $ g `at` (ro,co)) [0..g_width g - 1]) [0..g_height g - 1]




