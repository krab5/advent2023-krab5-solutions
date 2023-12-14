module Geometry.Movement where

data Direction = North | East | West | South deriving (Eq,Ord,Enum,Show)

vertical :: Direction -> Bool
vertical North = True
vertical South = True
vertical _ = False

horizontal :: Direction -> Bool
horizontal East = True
horizontal West = True
horizontal _ = False

opp :: Direction -> Direction
opp North = South
opp South = North
opp East  = West
opp West  = East

move :: (Int,Int) -> Direction -> (Int,Int)
move (ro,co) North = (ro - 1, co    )
move (ro,co) South = (ro + 1, co    )
move (ro,co) East  = (ro    , co + 1)
move (ro,co) West  = (ro    , co - 1)

neighbors :: (Int,Int) -> [(Direction,(Int,Int))]
neighbors x =
    map (\d -> (d, move x d)) [North,East,West,South]


