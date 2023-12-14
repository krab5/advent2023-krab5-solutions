{-|
Module: Geometry.Grid.Mutable
Description: Grid system (mutable)
Copyright: (c) krab5, 2023
License: GPL-3
Maintainer: crab.delicieux@gmail.com

This small module implements a grid system with default value (for edge cases) and
with an underlying boxed mutable efficient vector.

It almost looks like a wrapper for vector with a coordinate transformation function,
plus some custom, practical functions on it.

-}
module Geometry.Grid.Mutable (
        Data.Vector.Mutable.PrimMonad,
        Data.Vector.Mutable.PrimState,
        module Geometry.Grid.Mutable
    ) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe, mapMaybe)
import Control.Monad
import Control.Monad.ST
import Data.Vector.Mutable (PrimMonad, PrimState)
import qualified Data.Vector.Mutable as V
import qualified Data.Vector as NMV
import qualified Geometry.Grid as NMG

-- | The type for the grid, associated to a context state (`RealWorld` or any s, if using
-- `IO` or `ST s` as the internal monad.
data GridM s a = GridM {
    g_default :: a,
    g_width :: Int,
    g_height :: Int,
    g_content :: V.MVector s a
}

-- | Determines if the coordinates are in the boundaries of the grid.
-- This operation is atomic (hence no execution context)
inBound :: GridM s a -> (Int,Int) -> Bool
inBound g (row,column) =
    0 <= row && row < g_height g && 0 <= column && column < g_width g

_id :: GridM s a -> (Int,Int) -> Maybe Int
_id g x
    | inBound g x = Just $ _uid g x
    | otherwise = Nothing

_uid :: GridM s a -> (Int,Int) -> Int
_uid g (row,column) = column + row * g_width g

_coord :: GridM s a -> Int -> (Int,Int)
_coord g i =
    i `divMod` (g_width g)

-- | Reads the value of the grid at the given coordinate.
get :: PrimMonad m => GridM (PrimState m) a -> (Int,Int) -> m a
get g c =
    case _id g c of
        Nothing -> return $ g_default g
        Just i -> V.read (g_content g) i

-- | Sets the value of the grid at the given coordinate (mutably!).
set :: PrimMonad m => GridM (PrimState m) a -> (Int,Int) -> a -> m ()
set g c v = 
    case _id g c of
        Nothing -> return ()
        Just i -> V.write (g_content g) i v

-- | Swap two values of the grid at the given coordinates (mutably).
swap :: PrimMonad m => GridM (PrimState m) a -> (Int,Int) -> (Int,Int) -> m ()
swap g c1 c2 =
    case (_id g c1, _id g c2) of
        (Nothing, Nothing) -> return ()
        (Nothing, Just i2) -> V.write (g_content g) i2 (g_default g)
        (Just i1, Nothing) -> V.write (g_content g) i1 (g_default g)
        (Just i1, Just i2) -> V.swap (g_content g) i1 i2

-- | Perform a bunch of sets at once (mutably)
sets :: PrimMonad m => GridM (PrimState m) a -> [((Int,Int), a)] -> m ()
sets g vs =
    let ss = mapMaybe (\(x,v) -> (,v) <$> _id g x) $ vs
    in forM_ ss $ uncurry $ V.write (g_content g)

-- | Iterate over the elements of the grid (from left to right then top to bottom) and
-- combine them with the function, which is also given the associated coordinate of each
-- element.
foldlc :: PrimMonad m => (b -> (Int,Int) -> a -> b) -> b -> GridM (PrimState m) a -> m b
foldlc f z g =
    V.ifoldl (\acc i x -> f acc (_coord g i) x) z $ g_content g

-- | Monadic version of `foldlc`, used for iterating and accessing the grid, typically.
foldMc :: PrimMonad m => (b -> (Int,Int) -> a -> m b) -> b -> GridM (PrimState m) a -> m b
foldMc f z g =
    V.ifoldM (\acc i x -> f acc (_coord g i) x) z $ g_content g

-- | Find the coordinates of an element satisfying the given predicate, or return nothing if no such
-- element exist.
findCoord :: PrimMonad m => (a -> Bool) -> GridM (PrimState m) a -> m (Maybe (Int,Int))
findCoord p g =
    find_ 0
    where vec = g_content g
          len = V.length vec
          find_ n 
              | n >= len = do
                  x <- V.read vec n
                  if p x then return (Just $ _coord g n) else find_ (n + 1)
              | otherwise = return Nothing

-- | Get the list of every (valid) coordinates of the grid, left to right and top to bottom.
allCoords :: GridM s a -> [(Int,Int)]
allCoords grid =
    [ (i, j) | i <- [0..g_width grid - 1]
             , j <- [0..g_height grid - 1] ]

-- | Parse a string into a grid using the provided reader
parseGrid :: PrimMonad m => (Char -> a) -> a -> String -> m (GridM (PrimState m) a)
parseGrid parseC defval ct =
    let ls@(l:_) = lines ct in
    let vec = NMV.fromList $ map parseC $ concat ls in do
        mvec <- NMV.thaw vec
        return $ GridM { g_width = length l
                       , g_height = length ls
                       , g_default = defval
                       , g_content = mvec }

-- | Transform the monad into a string for pretty printing
pretty :: PrimMonad m => (a -> Char) -> GridM (PrimState m) a -> m String
pretty showC g =
    foldlc show1 "" g
    where lastc = g_width g - 1
          lastr = g_height g - 1
          show1 acc (r,c) x = acc ++ (showC x):(if c == lastc && r < lastr then "\n" else "")

-- | Transform an immutable grid into a mutable one (by thawing the internal vector)
thawGrid :: PrimMonad m => NMG.Grid a -> m (GridM (PrimState m) a)
thawGrid nmg = do
    mvec <- NMV.thaw (NMG.g_content nmg)
    return $ GridM { g_width = NMG.g_width nmg
                   , g_height = NMG.g_height nmg
                   , g_default = NMG.g_default nmg
                   , g_content = mvec }

-- | Freeze a mutable grid into an immutable one (which makes a copy of the internal
-- vector).
freezeGrid :: PrimMonad m => GridM (PrimState m) a -> m (NMG.Grid a)
freezeGrid mg = do
    fvec <- NMV.freeze (g_content mg)
    return $ NMG.Grid { NMG.g_width = g_width mg
                      , NMG.g_height = g_height mg
                      , NMG.g_default = g_default mg
                      , NMG.g_content = fvec }

-- | Clone an immutable grid (by cloning its internal vector)
cloneGrid :: PrimMonad m => GridM (PrimState m) a -> m (GridM (PrimState m) a)
cloneGrid g = do
    cg <- V.clone (g_content g)
    return $ GridM { g_width = g_width g
                   , g_height = g_height g
                   , g_default = g_default g
                   , g_content = cg }



