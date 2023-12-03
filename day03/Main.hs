module Main where

import Machine
import Main.Base
import Text.Printf
import Data.Maybe (mapMaybe)

process1 :: Logger -> String -> IO ()
process1 logger ct =
    let grid = parseGrid ct in do
        logger (printf "Parsed grid: area = %s, %d part candidates, %d symbols" (show $ area grid) (length $ parts grid) (length $ symbols grid))
        let okparts = filter (partOk $ symbols grid) (parts grid) in do
            logger (printf "Found %d actual parts: %s" (length okparts) (show (map part_id okparts)))
            logger (printf "Sum of part IDs: %d" (sum (map part_id okparts)))
            let gears = mapMaybe (isGear okparts) (symbols grid) in do
                logger (printf "Found %d gears" (length gears))
                logger (printf "Sum of gear ratios: %d" (sum gears))

main :: IO ()
main = doMain process1

