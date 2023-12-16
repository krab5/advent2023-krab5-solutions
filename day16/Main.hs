module Main where

import Main.Base
import Control.Monad
import Text.Printf
import Geometry.Movement
import Mirror
import Beam
import Data.List (maximumBy)
import Data.Function (on)

search :: Logger -> Int -> Int -> Positions -> IO ((Int,Int),Direction,Int)
search logger rows cols mirrors = do
    results <- mapM f1 possibilities
    let m = maximumBy (compare `on` trd) results
    return m
    where possibilities =
              [ ((0,i),South) | i <- [0..cols - 1] ]
              ++ [ ((rows-1,i),North) | i <- [0..cols - 1] ]
              ++ [ ((i,0),East) | i <- [0..rows - 1] ]
              ++ [ ((i,cols-1),West) | i <- [0..cols - 1] ]
          f1 (p,d) = 
              let result = beam (initGrid rows cols) mirrors p d in 
              let beaml = beamLength result in do
                  logger (printf " - Starting %s from %s => beam length = %d" (show d) (show p) beaml)
                  return (p,d,beaml)
          trd (_,_,x) = x

process1 :: Logger -> String -> IO ()
process1 logger ct = do
    logger (printf "Area size: %d×%d, found %d mirrors" rows cols (numMirrors mirrors))
    logger (printf "Mirrors:\n%s" ct)
    logger (printf "Beam (starting at (0,0, going east)):\n%s" (prettyBeams result))
    logger (printf "Beam length: %d" beaml)
    logger ("Looking for longest beam...")
    (pos,dir,len) <- search logger rows cols mirrors
    logger (printf "Found best length %d, starting at %s in direction %s" len (show pos) (show dir))
    where ((rows,cols),mirrors) = getPositions ct
          result = beam (initGrid rows cols) mirrors (0,0) East
          beaml = beamLength result


main :: IO ()
main = doMain process1


