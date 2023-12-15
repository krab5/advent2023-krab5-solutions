module HASHMAP.Instr where

import HASHMAP.Boxes

data Op = Remove | Add Int deriving Show
data Instr = Instr { i_hash :: Int, i_label :: String, i_op :: Op } deriving Show

hash :: [Char] -> Int
hash =
    foldl hash1 0
    where hash1 h x = ((h + fromEnum x) * 17) `mod` 256

mkInstr :: String -> Op -> Instr
mkInstr label op = Instr { i_hash = hash label, i_label = label, i_op = op }

execute :: Boxes -> Instr -> Boxes
execute boxes i =
   setBox boxes (i_hash i) op
   where op =
             case i_op i of
                 Remove -> removeLens (i_label i)
                 Add n -> addLens (mkLens (i_label i) n)




