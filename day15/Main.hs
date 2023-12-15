module Main where

import Main.Base
import Parsing
import Parsing.Stream
import Control.Applicative
import Control.Monad
import Text.Printf
import Data.Maybe (fromMaybe)
import qualified Data.Char as Char
import HASHMAP.Parsing
import HASHMAP.Boxes
import HASHMAP.Instr


process1 :: Logger -> (Int,[Instr]) -> IO ()
process1 logger (hh,instrs) = do
    logger (printf "Got %d instructions" (length instrs))
    logger (printf "Total hash: %d" hh)
    logger ("Last box state:\n" ++ pretty result)
    logger (printf "Total focusing power: %d" (boxes_power result))
    where result = foldl execute emptyBoxes instrs
          resultM = foldM executeM emptyBoxes instrs
          executeM acc x = do
              logger ("Current state:\n" ++ pretty acc)
              return $ execute acc x

preerr1 :: Logger -> Maybe a -> IO ()
preerr1 logger _ = logger "Parse error."

preproc1 :: Logger -> String -> IO (Maybe (Int,[Instr]))
preproc1 logger ct =
    return $ do 
        hh <- execParser parseHash (ignoring Char.isSpace ct)
        is <- execParser parse (ignoring Char.isSpace ct)
        return (hh, is)

main :: IO ()
main = doMainPre preproc1 preerr1 process1


