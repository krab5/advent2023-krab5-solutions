module Main where

import Main.Base
import Parsing
import Control.Applicative
import Control.Monad
import Text.Printf
import qualified Data.Char as Char
import qualified Data.Map as M

data LR = L | R deriving (Eq,Ord,Enum,Show)
type Node = String
type Graph = M.Map Node (Node,Node)

parseDir :: Parser [Char] [LR]
parseDir =
    (parseNewline >> return [])
        <|> (((parseChar_ 'L' >> return L) <|> (parseChar_ 'R' >> return R)) %:> parseDir)

parseGraph :: Parser [Char] Graph
parseGraph =
    foldlP (\map (k,lr) -> M.insert k lr map) M.empty parseEntry
    where parseNode = (parseP Char.isAlphaNum `ptimes` 3)
          parseEntry = 
              parseNewline >> parseNode >>= \src -> parseSpaces' >> parseChar '=' >> parseSpaces' >> 
                  parseChar '(' >> parseNode >>= \left -> parseChar ',' >> parseChar ' ' >> parseNode >>= 
                      \right -> parseChar ')' >> return (src,(left,right))

parse :: Parser [Char] ([LR],Graph)
parse =
    parseDir >>= \dirs -> parseGraph >>= \graph -> return (dirs, graph)

takeDir :: LR -> (a, a)  -> a
takeDir L (l,_) = l
takeDir R (_,r) = r

move :: Graph -> LR -> Node -> Maybe Node
move graph dir src = 
    M.lookup src graph >>= (return . takeDir dir)

step :: Graph -> [LR] -> (Node -> Bool) -> Node -> Maybe (Node,Integer)
step graph dirs isFinal src =
    step_ 0 dirs src
    where step_ acc (d:ds) n 
              | isFinal n = Just (n,acc)
              | otherwise = move graph d n >>= step_ (acc + 1) ds

findInit :: Graph -> [Node]
findInit = filter (\[_,_,x] -> x == 'A') . M.keys

isFinal :: Node -> Bool
isFinal [_,_,x] = x == 'Z'

process1 :: Logger -> String -> IO ()
process1 logger ct = 
    case execParser parse ct of
      Nothing -> logger "Parse error"
      Just (dirs,graph) -> do
          logger (printf "Parsed %d directions, size of graph is %d" (length dirs) (M.size graph))
          case step graph (cycle dirs) (=="ZZZ") "AAA" of
            Nothing -> logger "No path from AAA to ZZZ!"
            Just (_,n) -> logger (printf "Length of path from AAA to ZZZ: %d" n)
          let init = findInit graph in do
              logger (printf "Found %d initial nodes" (length init))
              let paths = map (step graph (cycle dirs) isFinal) init in do
                  result <- foldM proc1 (Just 1) (zip init paths)
                  case result of
                    Nothing -> logger "There is no solution to the problem"
                    Just n -> logger (printf "A solution is reachable in %d steps" n)
    where proc1 acc (init,Nothing) = logger (printf " - No solution from %s" init) >> return Nothing
          proc1 acc (init,Just (node,steps)) = do
              logger (printf " - %s => %s in %d steps" init node steps)
              return (acc >>= (return . lcm steps))

main :: IO ()
main = doMain process1


