module Main where

import Data.List
import Pannel
import Geometry.Movement (Direction(..))
import Main.Base
import Control.Monad
import Control.Monad.ST
import Text.Printf

processP1 :: Logger -> String -> IO ()
processP1 logger ct = do
    pannel <- parsePannel ct
    sp <- showPannel pannel
    logger (printf "Parsed pannel (%d×%d)\n%s" (width pannel) (height pannel) (sp))
    (spN,ldN) <- do1dir pannel North
    (spE,ldE) <- do1dir pannel East
    (spS,ldS) <- do1dir pannel South
    (spW,ldW) <- do1dir pannel West
    if width pannel < 15 then
        let allinone = assemble [spN,spE,spS,spW] in
            logger (printf "\n   NORTH        EAST         SOUTH        WEST   \n%s" allinone)
    else return ()
    logger (printf "Load bearing on north: %d" ldN)
    logger (printf "Load bearing on east : %d" ldE)
    logger (printf "Load bearing on south: %d" ldS)
    logger (printf "Load bearing on west : %d" ldW)
    do1Cycle pannel
    sp <- showPannel pannel
    logger (printf "After 1 cycle:\n%s" sp)
    where do1dir pannel dir = do
              p <- clonePannel pannel
              tilt dir p
              ld <- calcLoad dir p
              sp <- showPannel p
              return (sp,ld)
          assemble = intercalate "\n" . map (intercalate "   ") . transpose . map lines


processP2 :: Logger -> String -> IO ()
processP2 logger ct = do
    pannel <- parsePannel ct
    logger "Attempting to find a cycle"
    result <- findCycle 1000000000 pannel
    case result of
        Left msg -> logger msg
        Right (start,size,hash) -> do
            logger (printf "Found a cycle starting at iteration %d, of size %d" start size)
            logger (printf "Load on north bearing: %d" (getNorthLoad hash))


process1 :: Logger -> String -> IO ()
process1 logger ct = do
    processP1 logger ct
    processP2 logger ct


main :: IO ()
main = doMain process1


