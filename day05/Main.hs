module Main where

import Range
import Main.Base
import Control.Applicative
import Control.Monad
import Parsing
import Text.Printf
import Data.List (intercalate)

------------
-- Data structures
------------

data MapRange = MapRange { 
    source_start :: Int, 
    destination_start :: Int, 
    maprange_span :: Int 
}

data Mapping = Mapping { 
    source :: String, 
    destination :: String, 
    map_ranges :: [MapRange] 
}

offset :: MapRange -> Int
offset mr = destination_start mr - source_start mr

source_range :: MapRange -> Range
source_range m = Range { range_start = source_start m, range_span = maprange_span m }

destination_range :: MapRange -> Range
destination_range m = Range { range_start = destination_start m, range_span = maprange_span m }

source_end :: MapRange -> Int
source_end mr = source_start mr + maprange_span mr

destination_end :: MapRange -> Int
destination_end mr = destination_start mr + maprange_span mr

instance Show MapRange where
  show m = 
      show (source_range m) ++ " -> " ++ show (destination_range m)

instance Show Mapping where
  show m =
      source m ++ " => " ++ destination m ++ ": " ++ (intercalate ", " $ map show $ map_ranges m)

apply_map :: [MapRange] -> Int -> Int
apply_map [] x = x
apply_map (r:rs) x = 
    let offset = x - source_start r in
        if offset < 0 || offset >= maprange_span r
            then apply_map rs x
            else destination_start r + offset

apply_mapping :: Mapping -> Int -> Int
apply_mapping mr = apply_map (map_ranges mr)

apply_mapping_all :: [Int] -> Mapping -> [Int]
apply_mapping_all l mr = map (apply_mapping mr) l

apply_map1_rs :: RangeSet -> MapRange -> (RangeSet,RangeSet)
apply_map1_rs rs mr =
    let (inrange,outrange) = extract (source_range mr) rs in
        (offset_all inrange (offset mr), outrange)

apply_map_rs :: RangeSet -> [MapRange] -> (RangeSet,RangeSet)
apply_map_rs rs [] = (empty_set,rs)
apply_map_rs rs (m:ms) =
    let (t,rs') = apply_map1_rs rs m in
        let (ts,rs'') = apply_map_rs rs' ms in
            (union t ts, rs'')

apply_mapping_all_rs :: RangeSet -> Mapping -> RangeSet
apply_mapping_all_rs rs m =
    let (t,rs') = apply_map_rs rs (map_ranges m) in
        union t rs'
              
seeds_to_rangeset :: [Int] -> RangeSet
seeds_to_rangeset [] = empty_set
seeds_to_rangeset (x1:x2:xs) =
    insert (Range x1 x2) (seeds_to_rangeset xs)

------------
-- Parsing
------------

parseRange :: Parser [Char] MapRange
parseRange = 
    parseNumber >>= \dst -> parseSpace' >> 
        parseNumber >>= \src -> parseSpace' >> 
            parseNumber >>= \siz -> parseSpaces0' >>
                parseNewline >> return (MapRange src dst siz)

parseMapping :: Parser [Char] Mapping
parseMapping =
    parseUntil (== '-') >>= \src -> parseSeq_ "-to-" >> parseUntil (== ' ') >>= \dest ->
        parseSeq_ " map:" >> parseNewline >> parseRanges >>= \rs ->
            return $ Mapping { source = src, destination = dest, map_ranges = rs }
    where parseRanges = (parseRange %:> parseRanges) <|> (return [])

parseAll :: Parser [Char] ([Int],[Mapping])
parseAll =
    (parseSeq_ "seeds:" >> foldrP (:) [] (parseSpace' >> parseNumber) >>> parseNewline) >>= \seeds ->
        parseMappings >>= \mappings -> return (seeds, mappings)
    where parseMappings = parseSpaces0 >> ((parseMapping %:> parseMappings) <|> (end >> return []))

------------
-- Main
------------
process1 :: Logger -> String -> IO ()
process1 logger ct =
    case execParser parseAll ct of
      Nothing -> logger "Parsing error"
      Just (seeds, mappings) -> do
          logger (printf "Seeds: %s" (show seeds))
          logger (printf "Found %d mappings" (length mappings))
          forM_ mappings $ \m ->
              logger (printf " - %s ;" (show m))
          logger "Doing the mapping"
          result <- foldM map1 seeds mappings
          logger (printf "Lowest location number: %d" (minimum result))
          let seedrange = seeds_to_rangeset seeds in do
              logger (printf "Got seeds on interval %s" (show $ set_span seedrange))
              result' <- foldM map2 seedrange mappings
              logger (printf "Lowest location: %d" (set_min result'))
    where map1 seeds mapping =
              let result = apply_mapping_all seeds mapping in do
                  logger (printf "Mapping %s => %s: %s" (source mapping) (destination mapping) (show result))
                  return result
          map2 seedrange mapping =
              let result = apply_mapping_all_rs seedrange mapping in do
                  logger (printf "Mapping %s => %s: %s" (source mapping) (destination mapping) (show $ set_span result))
                  return result

main :: IO ()
main = doMain process1


