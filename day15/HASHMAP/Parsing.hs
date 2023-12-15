module HASHMAP.Parsing where

import Parsing
import Parsing.Stream
import Control.Applicative
import Control.Monad
import qualified Data.Char as Char

import HASHMAP.Instr

parse :: Stream s => Parser (s Char) [Instr]
parse =
    parseInstr %:> repeatP (parseChar_ ',' >> parseInstr)
    where parseInstr = 
              parseWhile Char.isAlpha >>= \label -> parseOp >>= \op -> return $ mkInstr label op
          parseOp =
              (parseChar_ '=' >> parseNumber >>= (return . Add))
              <|> (parseChar_ '-' >> return Remove)

parseHash :: Stream s => Parser (s Char) Int
parseHash =
    parseNotComma >>= \a -> 
        foldlP (\acc str -> acc + (hash str)) (hash a) (parseChar_ ',' >> parseNotComma) 
    where parseNotComma = parseUntil (== ',') <|> (end >> return "")


