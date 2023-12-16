module Beam where

import Data.Word
import Data.Bits
import Geometry.Movement
import Geometry.Grid

-- sswweenn => beam in each direction, either 0, 1 (simple), 2 (powerful)
data Beam = Beam Word8 deriving (Eq,Ord)

no_beam :: Beam
no_beam = Beam 0

beam_power :: Beam -> Direction -> Int
beam_power (Beam b) dir = fromIntegral $ (b `shiftR` (2 * fromEnum dir)) .&. 3

put_beam :: Beam -> Direction -> Beam
put_beam (Beam b) dir =
    let order = 2 * fromEnum dir in
    let value = min (((b `shiftR` order) .&. 3) + 1) 2
        mask  = 0xFF `xor` (0x3 `shiftL` order) in
        Beam $ (b .&. mask) .|. (value `shiftL` order)

instance Show Beam where
    show b =
        "["
        ++ "N " ++ show (beam_power b North)
        ++ " | E " ++ show (beam_power b East)
        ++ " | S " ++ show (beam_power b South)
        ++ " | W " ++ show (beam_power b West)
        ++ "]"

type BeamGrid = Grid Beam

initGrid :: Int -> Int -> BeamGrid
initGrid width height = mkGrid width height no_beam no_beam

putBeam :: BeamGrid -> (Int,Int) -> Direction -> BeamGrid
putBeam grid pos dir =
    modify grid pos (\b -> put_beam b dir)

beamLength :: BeamGrid -> Int
beamLength grid =
    foldlc (\acc _ b -> if b == no_beam then acc else acc + 1) 0 grid

beamToChar :: Beam -> Char
beamToChar b =
    charfor (beam_power b South) (beam_power b West) (beam_power b East) (beam_power b North)
    where --      S W E N
          charfor 0 0 0 0 = ' '
          charfor 0 1 1 0 = '─'
          charfor 0 2 2 0 = '━'
          charfor 1 0 0 1 = '│'
          charfor 2 0 0 2 = '┃'
          charfor 1 0 1 0 = '┌'
          charfor 1 0 2 0 = '┍'
          charfor 2 0 1 0 = '┎'
          charfor 2 0 2 0 = '┏'
          charfor 1 1 0 0 = '┐'
          charfor 1 2 0 0 = '┑'
          charfor 2 1 0 0 = '┒'
          charfor 2 2 0 0 = '┓'
          charfor 0 0 1 1 = '└'
          charfor 0 0 2 1 = '┕'
          charfor 0 0 1 2 = '┖'
          charfor 0 0 2 2 = '┗'
          charfor 0 1 0 1 = '┘'
          charfor 0 2 0 1 = '┙'
          charfor 0 1 0 2 = '┚'
          charfor 0 2 0 2 = '┛'
          charfor 1 0 1 1 = '├'
          charfor 1 0 2 1 = '┝'
          charfor 1 0 1 2 = '┞'
          charfor 2 0 1 1 = '┟'
          charfor 2 0 1 2 = '┠'
          charfor 1 0 2 2 = '┡'
          charfor 2 0 2 1 = '┢'
          charfor 2 0 2 2 = '┣'
          charfor 1 1 0 1 = '┤'
          charfor 1 2 0 1 = '┥'
          charfor 1 1 0 2 = '┦'
          charfor 2 1 0 1 = '┧'
          charfor 2 1 0 2 = '┨'
          charfor 1 2 0 2 = '┩'
          charfor 2 2 0 1 = '┪'
          charfor 2 2 0 2 = '┫'
          charfor 1 1 1 0 = '┬'
          charfor 1 2 1 0 = '┭'
          charfor 1 1 2 0 = '┮'
          charfor 1 2 2 0 = '┯'
          charfor 2 1 1 0 = '┰'
          charfor 2 2 1 0 = '┱'
          charfor 2 1 2 0 = '┲'
          charfor 2 2 2 0 = '┳'
          charfor 0 1 1 1 = '┴'
          charfor 0 2 1 1 = '┵'
          charfor 0 1 2 1 = '┶'
          charfor 0 2 2 1 = '┷'
          charfor 0 1 1 2 = '┸'
          charfor 0 2 1 2 = '┹'
          charfor 0 1 2 2 = '┺'
          charfor 0 2 2 2 = '┻'
          charfor 1 1 1 1 = '┼'
          charfor 1 2 1 1 = '┽'
          charfor 1 1 2 1 = '┾'
          charfor 1 2 2 1 = '┿'
          charfor 1 1 1 2 = '╀'
          charfor 2 1 1 1 = '╁'
          charfor 2 1 1 2 = '╂'
          charfor 1 2 1 2 = '╃'
          charfor 1 1 2 2 = '╄'
          charfor 2 2 1 1 = '╅'
          charfor 2 1 2 1 = '╆'
          charfor 1 2 2 2 = '╇'
          charfor 2 2 2 1 = '╈'
          charfor 2 2 1 2 = '╉'
          charfor 2 1 2 2 = '╊'
          charfor 2 2 2 2 = '╋'
          charfor 0 0 1 0 = '╴'
          charfor 0 0 0 1 = '╵'
          charfor 0 1 0 0 = '╶'
          charfor 1 0 0 0 = '╷'
          charfor 0 0 2 0 = '╸'
          charfor 0 0 0 2 = '╹'
          charfor 0 2 0 0 = '╺'
          charfor 2 0 0 0 = '╻'
          charfor 0 1 2 0 = '╼'
          charfor 2 0 0 1 = '╽'
          charfor 0 2 1 0 = '╾'
          charfor 1 0 0 2 = '╿'
          charfor _ _ _ _ = '?'

prettyBeams :: BeamGrid -> String
prettyBeams = pretty beamToChar




