module Mirror where

import Geometry.Grid
import Geometry.Movement
import qualified Data.Map as M
import qualified Data.Set as S

import Beam

data Mirror = HSplit | VSplit | Slash | BSlash deriving (Eq,Ord,Enum,Show)

enter :: Mirror -> Direction -> [Direction]
enter HSplit d
    | horizontal d = [opp d]
    | otherwise = [East,West]
enter VSplit d
    | vertical d = [opp d]
    | otherwise = [North,South]
enter Slash  East  = [South]
enter Slash  West  = [North]
enter Slash  North = [West]
enter Slash  South = [East]
enter BSlash East  = [North]
enter BSlash West  = [South]
enter BSlash North = [East]
enter BSlash South = [West]

type Positions = M.Map (Int,Int) Mirror
type Hits = S.Set (Direction,(Int,Int))

numMirrors :: Positions -> Int
numMirrors = M.size

getPositions :: String -> ((Int,Int),Positions)
getPositions ct =
    let lns@(l:_) = lines ct in
    let poss = snd $ foldl (\(ro,acc) ln -> 
                   (ro + 1, snd $ foldl (\(co,acc) x -> 
                       (co + 1, parse acc (ro,co) x)) (0,acc) ln)) (0,M.empty) lns
    in ((length lns, length l),poss)
    where parse acc pos '/' = M.insert pos Slash acc
          parse acc pos '\\' = M.insert pos BSlash acc
          parse acc pos '-' = M.insert pos HSplit acc
          parse acc pos '|' = M.insert pos VSplit acc
          parse acc _   _   = acc

beam :: BeamGrid -> Positions -> (Int,Int) -> Direction -> BeamGrid
beam grid mirrors start dir =
    fst $ beam_ grid S.empty start dir 
    where beam_ :: BeamGrid -> Hits -> (Int,Int) -> Direction -> (BeamGrid,Hits)
          beam_ grid hits pos dir 
              | not (inBound grid pos) = (grid,hits)
              | otherwise =
                  let grid' = putBeam grid pos (opp dir) in -- beam at entry
                      case M.lookup pos mirrors of
                          Nothing -> beam_ (putBeam grid' pos dir) hits (move pos dir) dir
                          Just _ | (dir,pos) `S.member` hits -> (grid,hits)
                          Just m ->
                              let hits' = S.insert (dir,pos) hits
                                  next = enter m (opp dir) in
                                  foldl (\(g,h) d -> beam_ (putBeam g pos d) h (move pos d) d) (grid', hits') next




