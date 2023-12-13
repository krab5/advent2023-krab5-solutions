module Geometry.Grid.Pretty where

import Geometry.Grid
import Data.List (intercalate)
import TermColor

-- | Pretty print the grid in a string with potential term colors (ANSI escape codes) depending on a set of
-- predicates. Useful for highlighting some parts of the grid, typically.
prettyMarkup :: (a -> Char) -> [((Int,Int) -> a -> Bool, TermColor)] -> Grid a -> String
prettyMarkup showC styleIf g =
    intercalate "\n" $ map concat $ map (\ro -> map (\co -> mkc (ro,co)) [0..g_width g - 1]) [0..g_height g - 1]
    where mkc x =
            let v = g `at` x in
                let c = showC v in
                    case findTc x v styleIf of
                        Nothing -> [c]
                        Just tc -> style tc [c]
          findTc x v [] = Nothing
          findTc x v ((p,t):ts)
            | p x v = Just t
            | otherwise = findTc x v ts
    


