module Main where

import Main.Base
import Pipes
import Movement
import Grid
import Text.Printf
import Control.Monad
import Data.Maybe (fromJust)
import qualified Data.Set as S

step :: PipeGrid -> (Direction,(Int,Int)) -> Maybe (Direction,(Int,Int))
step g (entered,c) = do
    p <- getPipe (g `at` c)
    o <- otherEnd p entered
    return (opp o, move c o)

-- It takes two to...
tango :: PipeGrid
      -> (Int,Int) -> Direction -> Direction
      -> Maybe Int
tango grid s d1 d2 =
    tangaux 1 (opp d1, move s d1) (opp d2, move s d2)
    where tangaux acc x1@(_,c1) x2@(_,c2)
              | c1 == c2 || c1 == s || c2 == s = Just acc -- In **theory**, only the first guard will ever be activated, given the topology of paths
              | otherwise = do -- Here I account for potential "nothings" in the browsing, but this should not happen (if the path is well-formed)
                  x1' <- step grid x1
                  x2' <- step grid x2
                  tangaux (acc+1) x1' x2'

getPath :: PipeGrid -> (Int,Int) -> Direction -> S.Set (Int,Int)
getPath grid s d =
    case aux (opp d, move s d) of -- Similarly, if the path is well-formed, this will never return Nothing.
        Nothing -> S.empty
        Just s -> s
    where aux x@(_,c)
            | c == s = Just $ S.singleton s
            | otherwise = step grid x >>= aux >>= (return . S.insert c)

-- Calculate the intersection of a line with a path, starting from x and going in direction d
intersect :: PipeGrid -> S.Set (Int,Int) -> (Int,Int) -> Direction -> Int
intersect g path x d =
    aux 0 x
    where aux :: Int -> (Int,Int) -> Int
          aux acc x
            | inBound g x = 
                let (n, x') = calcIntersect x in
                    aux (acc + n) x'
            | otherwise = acc
          calcIntersect :: (Int,Int) -> (Int,(Int,Int))
          calcIntersect x
            | x `S.member` path =
                let (APipe p) = g `at` x in -- in **theory**, as x is in the path, the tile is pipe tile
                    case if horizontal d then verticalComponent p else horizontalComponent p of
                        Nothing -> (1, move x d) -- The pipe is straight (nobody's perfect), presumably orthogonal to the direction, so it counts for 1
                        Just turn -> eat turn (move x d) -- The pipe is turning, we need to determine which directtion it turns next (away or in the same direction)
            | otherwise = (0, move x d)
          eat :: Direction -> (Int,Int) -> (Int,(Int,Int))
          eat turn x = 
            let (APipe p) = g `at` x in -- in **theory**, since we go allong a path between two turns, we only have pipe tiles, so this is safe
                case if horizontal d then verticalComponent p else horizontalComponent p of
                    Nothing -> eat turn (move x d) -- the pipe is straight, we need to go further
                    Just turn' -> (if turn == turn' then 2 else 1, move x d) -- if the pipe turns in the same direction, count as 2, otherwise count as 1

-- Determine if the coordinate is inside the area delimited by the path.
-- Consider a line in each direction and count the dumber of times it crosses the path.
-- If that number is odd for each direction, then the point is inside.
isInside :: PipeGrid -> S.Set (Int,Int) -> (Int,Int) -> Bool
isInside grid path c
    | c `S.member` path = False -- points *on* the path are never inside (this simple guard here removes probably half the points to check)
    | otherwise =
        let north = intersect grid path c North
            south = intersect grid path c South
            east  = intersect grid path c East
            west  = intersect grid path c West
            in odd north && odd south && odd east && odd west
        where odd x = x `mod` 2 == 1

-- | Get intersection for every points. Kinda brute-force not gonna lie, but it works very well.
getInside :: PipeGrid -> S.Set (Int,Int) -> S.Set (Int,Int)
getInside grid path =
    S.fromList $ filter (isInside grid path) $ allCoords grid

process1 :: Logger -> (PipeGrid,(Int,Int),Direction,Direction) -> IO ()
process1 logger (g,start,d1,d2) = do 
    logger (printf "Parsed grid (%d×%d):" (g_width g) (g_height g))
    forM_ (lines $ prettyGrid) logger
    logger (printf "Path size: %d" (S.size path))
    logger (printf "Number of tiles inside: %d" (S.size inside))
    case tango g start d1 d2 of
        Nothing -> logger "I could not find a suitable path..."
        Just n -> logger (printf "Number of steps to reach the farthest point: %d" n)
    return ()
    where path = getPath g start d1
          inside = getInside g path
          -- HERE COMES BY GRIIIIIID, COULD SHE BE ANY CUTER????
          prettyGrid = 
              prettyMarkup showTile [(_isStart,mkBackground 90)
                                    ,(_inPath,mkBackground 34)
                                    ,(_inside,mkBackground 220)] g
          _isStart x _ = x == start
          _inPath x _ = x `S.member` path
          _inside x _ = x `S.member` inside


-- | I consider the preprocessing failed if I cannot find the start. Normally there is always
-- a starting point because input provided by the challenge are well-formed, but this is
-- to have a somewhat safe code.
preproc1 :: Logger -> String -> IO (Maybe (PipeGrid,(Int,Int),Direction,Direction))
preproc1 logger ct =
    let grid = parsePipeGrid ct in 
        return (getStart grid >>= \s -> getConnect grid s >>= \(x1,x2) -> 
            return (set grid s (APipe $ mkPipe x1 x2),s,x1,x2))
    where getConnect g s =
            case connectedTo g s of
                [x1,x2] -> Just (x1,x2)
                _ -> Nothing

preprocerr :: Logger -> Maybe a -> IO ()
preprocerr logger Nothing = logger "Could not find a starting point in the grid :("

main :: IO ()
main = doMainPre preproc1 preprocerr process1


