module Pipes where

import Movement
import Geometry.Grid

data Pipe = Pipe { end1 :: Direction, end2 :: Direction }

showPipe :: Pipe -> Char
showPipe (Pipe e1 e2) =
    (rep !! fromEnum e1) !! fromEnum e2
    where rep = ["?╚╝║"  -- N
                ,"╚?═╔"  -- E
                ,"╝═?╗"  -- W
                ,"║╔╗?"] -- S
            --    NEWS

instance Show Pipe where
    show (Pipe e1 e2) = show e1 ++ " <=> " ++ show e2

instance Eq Pipe where
    (Pipe e1 e2) == (Pipe e1' e2') =
        (e1 == e1' && e2 == e2') || (e1 == e2' && e2 == e1')

mkPipe :: Direction -> Direction -> Pipe
mkPipe e1 e2 = Pipe (min e1 e2) (max e1 e2)

connectedD :: Pipe -> Direction -> Bool
connectedD (Pipe e1 e2) d =
    e1 == d || e2 == d

connected :: Pipe -> Pipe -> Bool
connected (Pipe e1 e2) (Pipe e1' e2') =
    e1 == e1' || e2 == e1' || e1 == e2' || e2 == e2'

otherEnd :: Pipe -> Direction -> Maybe Direction
otherEnd (Pipe e1 e2) d
    | d == e1 = Just e2
    | d == e2 = Just e1
    | otherwise = Nothing

straight :: Pipe -> Bool
straight (Pipe e1 e2) = e1 == opp e2

isHorizontal :: Pipe -> Bool
isHorizontal (Pipe East West) = True
isHorizontal (Pipe West East) = True
isHorizontal _ = False

isVertical :: Pipe -> Bool
isVertical (Pipe North South) = True
isVertical (Pipe South North) = True
isVertical _ = False

verticalComponent :: Pipe -> Maybe Direction
verticalComponent (Pipe e1 e2)
    | vertical e1 && horizontal e2 = Just e1
    | vertical e2 && horizontal e1 = Just e2
    | otherwise = Nothing

horizontalComponent :: Pipe -> Maybe Direction
horizontalComponent (Pipe e1 e2)
    | horizontal e1 && vertical e2 = Just e1
    | horizontal e2 && vertical e1 = Just e2
    | otherwise = Nothing


data Tile = APipe Pipe | Start | Ground deriving Show

isStart :: Tile -> Bool
isStart Start = True
isStart _ = False

isGround :: Tile -> Bool
isGround Ground = True
isGround _ = False

isPipe :: Tile -> Bool
isPipe (APipe _) = True
isPipe _ = False

getPipe :: Tile -> Maybe Pipe
getPipe (APipe p) = Just p
getPipe _ = Nothing

parsePipe :: Char -> Tile
parsePipe '|' = APipe $ mkPipe North South
parsePipe '-' = APipe $ mkPipe East West
parsePipe 'L' = APipe $ mkPipe North East
parsePipe 'J' = APipe $ mkPipe North West
parsePipe '7' = APipe $ mkPipe West South
parsePipe 'F' = APipe $ mkPipe East South
parsePipe 'S' = Start
parsePipe _ = Ground

showTile :: Tile -> Char
showTile Start = 'S'
showTile Ground = '.'
showTile (APipe p) = showPipe p


type PipeGrid = Grid Tile

prettyPipe :: PipeGrid -> String
prettyPipe = pretty showTile

parsePipeGrid :: String -> PipeGrid
parsePipeGrid ct = parseGrid parsePipe Ground ct

getStart :: PipeGrid -> Maybe (Int,Int)
getStart = findCoord isStart

connectedTo :: PipeGrid -> (Int,Int) -> [Direction]
connectedTo g s =
    map fst $ aux s
    where aux s = filter hasConn $ neighbors s
          hasConn (d,c) = 
            case g `at` c of
                APipe p -> connectedD p (opp d)
                _ -> False

guessStart :: PipeGrid -> Maybe Pipe
guessStart grid =
    case connectedTo grid <$> getStart grid of
        Just [d1,d2] -> Just $ mkPipe d1 d2
        _ -> Nothing


