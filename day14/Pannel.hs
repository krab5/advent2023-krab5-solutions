module Pannel where

import Geometry.Movement
import Geometry.Grid.Mutable
import Data.Function ((&))
import Control.Monad
import qualified Data.Map as M

data Place = Ball | Wall | Hole deriving (Eq,Ord,Enum,Show)
-- A pannel is a grid of places, surrounded by walls (using the Grid module default value
-- system). The grid is mutable for efficiency.
type Pannel s = GridM s Place

-- Clone a pannel entirely, usually for performing different computations with it
clonePannel :: PrimMonad m => Pannel (PrimState m) -> m (Pannel (PrimState m))
clonePannel p = cloneGrid p

width :: Pannel s -> Int
width = g_width

height :: Pannel s -> Int
height = g_height

-- Find the coordinate of the next obstacle (wall or other ball)
nextObstacle :: PrimMonad m => Pannel (PrimState m) -> (Int,Int) -> Direction -> m (Int,Int)
nextObstacle pannel start dir =
    let next = move start dir in do
        val <- get pannel next
        if val == Hole then nextObstacle pannel next dir else return start

-- Move the current element (a ball, presumably) all the way to be right before the
-- next obstacle, if possible. 
moveAllTheWay :: PrimMonad m => Direction -> Pannel (PrimState m) -> (Int,Int) -> m ()
moveAllTheWay dir pannel elt = do
    next <- nextObstacle pannel elt dir
    if next /= elt then
        swap pannel next elt
    else
        return ()

-- Move each element (that can be moved, i.e. balls) of the line to their max position
-- on the orthogonal line.
moveLineAllTheWay :: PrimMonad m => Direction -> Pannel (PrimState m) -> Int -> m ()
moveLineAllTheWay dir pannel line =
    mapM_ move_ places
    where places =
            if horizontal dir then 
                map (,line) [0..g_height pannel - 1]
            else
                map (line,) [0..g_width pannel - 1]
          move_ c = do
              val <- get pannel c
              if val == Ball then moveAllTheWay dir pannel c else return ()

-- Tilt the pannel one direction, until every ball stop moving.
tilt :: PrimMonad m => Direction -> Pannel (PrimState m) -> m ()
tilt dir pannel =
    mapM_ (moveLineAllTheWay dir pannel) $ getLines dir
    where getLines West = [0..g_width pannel - 1]
          getLines East = reverse $ getLines West
          getLines North = [0..g_height pannel - 1]
          getLines South = reverse $ getLines North

-- Tentative (successfull) for a hash of a grid, that contains the information of the load
-- on each side.
data Hash = Hash {
    getNorthLoad :: Int,
    getWestLoad  :: Int,
    getSouthLoad :: Int,
    getEastLoad  :: Int 
} deriving (Eq,Ord)

-- Calculate the hash of a pannel. This is presumably injective because of the load calculation
-- system (not proven, just a hunch).
hash :: PrimMonad m => Pannel (PrimState m) -> m Hash
hash pannel = do
    lpN <- calcLoad North pannel
    lpW <- calcLoad West  pannel
    lpS <- calcLoad South pannel
    lpE <- calcLoad East  pannel
    return $ Hash lpN lpW lpS lpE

-- Do 1 cycle of tilt (N > W > S > E)
do1Cycle :: PrimMonad m => Pannel (PrimState m) -> m ()
do1Cycle pannel = do
    tilt North pannel
    tilt West  pannel
    tilt South pannel
    tilt East  pannel

-- FInd a cycle in a sequence of tilting, then calculate the hash of the pannel that would have been
-- found at cycle n.
--
-- This is done by storing the hash of the current pannel at every point (the "past"). When calcualting
-- a new pannel state, we compute its hash and find it in the past. If it does not exist, we continue,
-- otherwise we find out the cycle size, determine the number of remaining iterations, then simply get
-- the hash corresponding that number modulo the cycle size.
findCycle :: PrimMonad m => Integer -> Pannel (PrimState m) -> m (Either String (Integer,Integer,Hash))
findCycle n pannel =
    aux M.empty 0
    where aux past k
              -- No cycle (let's hope we don't get here)
              | k >= n = return $ Left "No cycle"
              | otherwise = do
                 h <- hash pannel
                 case M.lookup h past of
                     Nothing -> do -- not a cycle yet, add the hash and compute following
                         do1Cycle pannel
                         aux (M.insert h k past) (k + 1)
                     Just it -> -- in the cycle! compute cycle size and get the correct hash
                         let cycleSize = k - it
                             remaining = n - k in
                         let offset = remaining `mod` cycleSize in
                             let ok = M.filter (== it + offset) past in 
                             case M.keys ok of
                                -- This is just to be safe because the other cases *cannot* happen
                                [x] -> return $ Right (it, cycleSize, x)
                                -- Since k > it + offset and the map has every value from 0 to k, this
                                -- canno happen
                                [] -> return $ Left "No cycle found"
                                -- Since there exists exactly one m such that past[h] = m (because each m 
                                -- is unique) this cannot happen either
                                l -> return $ Left $ "Multiple cycles found (" ++ show (length l) ++ ")"


-- Calculate the load born by one of the direction on the pannel
calcLoad :: PrimMonad m => Direction -> Pannel (PrimState m) -> m Int
calcLoad dir pannel =
    foldlc score1 0 pannel
    where score1 acc c Ball = acc + scoreBall dir c
          score1 acc _ _ = acc
          scoreBall North (ro, _) = g_height pannel - ro
          scoreBall South (ro, _) = ro + 1
          scoreBall East  (_ ,co) = co + 1
          scoreBall West  (_ ,co) = g_width pannel - co

parsePannel :: PrimMonad m => String -> m (Pannel (PrimState m))
parsePannel = 
    parseGrid parse1 Wall 
    where parse1 '#' = Wall
          parse1 'O' = Ball
          parse1 _ = Hole

showPannel :: PrimMonad m => Pannel (PrimState m) -> m String
showPannel =
    pretty show1
    where show1 Wall = '#'
          show1 Hole = '.'
          show1 Ball = 'O'





