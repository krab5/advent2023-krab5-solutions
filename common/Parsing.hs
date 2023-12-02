{-|
Module: Parsing
Description: A "small" parser combinator library
Copyright: (c) krab5, 2023
License: GPL-3
Maintainer: crab.delicieux@gmail.com

This library implements a simple-yet-versatile form of parser combinators, represented basically
as state monads. Parsers are alternative monads, with `<|>` being the choice (alternative in a
grammar) and `>>=` being the sequencing of parsers.

The module also provides alternative functions and operators, for convenience. For instance,
`%>` and `%:>` exploit parsing results that are `MonadPlus` to parse and accumulate results
(which is very common when having repetitions, for instance).

The library has been made fairly generic, in particular in term of how the parsing result is
made. A "default" implementation uses the `Maybe` monad, which is good enough for simple parsing.

-}
module Parsing where

import Data.Maybe (isNothing)
import Data.Functor
import Control.Applicative
import Control.Monad
import qualified Data.Char as Char

-- | The main type for the parser, or parser "transformer". The first type parameter (`m`) is the
-- monad wrapping the parsing result. It should be some kind of alternative monad or `MonadFail` in 
-- theory. This is not mandatory but will greatly impact the lib's usability.
--
-- The second type parameter (`s`) is the incoming stream. Once again, this could really be anything,
-- although a number of convenient functions work on _stream-like_ inputs (see `Stream` typeclass).
--
-- Last, the third type parameter (`r`) is the type of the outgoing result. This is the type that varies
-- the most and for which `ParserT` is a well-behave monad.
newtype ParserT m s r = ParserT { 
    runParserT :: s -> m (s, r)     -- ^ Unwrap the parser, i.e. runs it
}

-- | Execute the parser, i.e. run it and keep only the result (if available). Note that no verification
-- is done as if there is still stream to be parsed. A good grammar should explicitly have a starting
-- rule with a `end` or similar terminal, indicating the end of stream.
execParserT :: Functor m => ParserT m s r -> s -> m r
execParserT p s = snd <$> runParserT p s

-- | `Functor` instance for the parser, straightforward
instance Functor m => Functor (ParserT m s) where
  fmap f p = ParserT $ \s -> fmap (fmap f) $ runParserT p s

-- | `Applicative` instance for the parser. 
instance Monad m => Applicative (ParserT m s) where
  -- | The parser that always succeed, does not consume any character and returns the provided result.
  pure x = ParserT $ \s -> return (s, x)
  -- | The applicative lift transfers whatever is left to be read from the flux from a parser to another.
  -- (In other words, this is not parallel parsing !)
  liftA2 f2 p1 p2 = -- (a -> b -> c) -> Parser s a -> Parser s b -> Parser s c
    ParserT $ \s ->
        runParserT p1 s >>= \(s', ra) -> runParserT p2 s' >>= \(s'', rb) -> return (s'', f2 ra rb)

-- | `Monad` instance for the parser.
instance Monad m => Monad (ParserT m s) where
  return = pure
  -- | This is effectively the sequencing of parsers with value
  -- transferring, allowing to accumulate values in a contexte, e.g. :
  -- > parse1 >>= \x -> parse2 >>= \y -> parse3 >>= \z -> return (x, y, z)
  -- 
  pa >>= f = ParserT $ \s ->
      runParserT pa s >>= \(s', ra) -> runParserT (f ra) s'

-- | `Alternative` instance for the parser. 
instance (Monad m, Alternative m) => Alternative (ParserT m s) where
  -- | The parser that parses nothing and always fails
  empty = ParserT $ \s -> empty
  -- | This is lazy "parallel" parsing: if the first parser
  -- succeeds, the others are not run; otherwise, we run the next, and so on. This allows to write
  -- grammars with alternative rules, e.g. :
  -- > -- A -> 'a'
  -- > -- A -> 'b'
  -- > parseA = parseChar 'a' <|> parseChar 'b'
  --
  p1 <|> p2 = ParserT $ \s -> runParserT p1 s <|> runParserT p2 s

-- | `MonadFail` instance for the parser. Useful to interrupt parsing with an error message.
-- Of course, it is more useful if the wrapping monad handles error messages correctly.
instance (MonadFail m) => MonadFail (ParserT m s) where
  fail x = ParserT $ \s -> fail x

-- | Type synonym for parsers which wrapping monad is `Maybe`.
type Parser = ParserT Maybe

-- | Run a parser with `Maybe` as its monad
runParser :: Parser s r -> s -> Maybe (s, r)
runParser = runParserT

-- | Create a parser from a function.
parserTOf :: (s -> m (s, r)) -> ParserT m s r
parserTOf = ParserT

-- | Same as `parserTOf` but specialized for parser with `Maybe`
parserOf :: (s -> Maybe (s, r)) -> Parser s r
parserOf = parserTOf

-- | A class representing a certain type of polymorphic data structure for which it is
-- possible to get an element + the rest of the structure (presumably without the element).
--
-- This class effectively factorises anything that looks like a sequence (or a list). It 
-- allows for a bit of flexibility when using parsers (rather than using plain old lists).
class Stream s where
  -- | Takes the stream and try to extract an element and the remainder of the stream. If not possible,
  -- return `Nothing`.
  --
  -- This function returning `Nothing` implies that the stream is finished (end of string, end of file,
  -- etc.) or at least that no other character is available.
  uncons :: s a -> Maybe (a, s a)

-- | Tests if the stream is done, i.e. it is empty/no other symbol is available
done :: Stream s => s a -> Bool
done = isNothing . uncons

-- | Stream instance for lists (the most useful one)
instance Stream [] where
  uncons [] = Nothing
  uncons (t:q) = Just (t, q)

-- | Parser that fails if the predicate is true on the given stream, and succeeds while consuming
-- nothing otherwise.
failIf :: Alternative m => (s -> Bool) -> ParserT m s ()
failIf p = ParserT $ \s -> if p s then empty else pure (s, ())

-- | Parser that always succeeds, consumes nothing and returns `mzero` in the provided `MonadPlus`
-- instance. This is especially useful for repetition, i.e. :
-- > -- A -> /\ (empty word)
-- > -- A -> x A
-- > parseA = (parseChar 'x' >>= \x -> parseA >>= \xs -> x `mplus` xs) <|> zero
--
zero :: (Monad m, MonadPlus f) => ParserT m s (f a)
zero = return mzero

-- | Parser that succeeds iff the stream has reach its end (and does not consume anything, subsequently).
end :: (Alternative m, Stream s) => ParserT m (s a) ()
end = failIf (not . done)

-- | Jump in the incoming stream, i.e. replace the current stream by another one. This is intended for
-- redirections during parsing, and especially resetting the incoming stream (when performing simple
-- lookahead).
--
-- This parser always succeeds, consume nothing and returns ()
jump :: Monad m => s -> ParserT m s ()
jump s = ParserT $ \_ -> return (s, ())

-- | Drop a symbol of the stream. This parser fails if the stream is done.
drop1 :: (Stream s, Alternative m) => ParserT m (s a) ()
drop1 = ParserT $ \s ->
    case uncons s of 
      Nothing -> empty
      Just (_,xs) -> pure (xs, ())

-- | Consume one symbol of the stream and return it as result (as a `MonadPlus`). Fails if the stream
-- is done.
--
-- This is intended for "transponder"-type parsers, which rewrites the input stream on their output (after
-- transformation). The result is a `MonadPlus` to allow to chain the `keep1`. The result is usually a
-- list as well in that case.
keep1 :: (Stream s, Alternative m, Applicative f) => ParserT m (s a) (f a)
keep1 = ParserT $ \s ->
    case uncons s of
      Nothing -> empty
      Just (x, xs) -> pure (xs, pure x)

-- | Parser that consumes nothing and fails if the stream is done/empty.
notEmpty :: (Alternative m, Stream s) => ParserT m (s a) ()
notEmpty = failIf done

-- | Parser combinator that runs the given parser and then sets back the stream where it
-- was before parsing.
--
-- For instance, if the result of parsing `onexyz...` is `1` with remaining stream being `xyz...`, then
-- using `parseAndBack` on this parser will yield the result `1` (result of the parser) but `onexyz...` as
-- the remaining stream.
parseAndBack :: Monad m => ParserT m s r -> ParserT m s r
parseAndBack p1 =
    ParserT $ \s -> runParserT (p1 >>= (\x -> jump s >> return x)) s

infixr 3 %>, %:>

-- | Convenient combinator for parsers which result is a `MonadPlus`, that chains the parsers and
-- `mplus` their results.
(%>) :: (Monad m, MonadPlus f) => ParserT m s (f a) -> ParserT m s (f a) -> ParserT m s (f a)
p1 %> p2 = p1 >>= \r1 -> p2 >>= \r2 -> return (r1 `mplus` r2)

-- | Specific version of `%>` where the first argument is not wrapped in the `MonadPlus` (convenient
-- for injecting an element in a list, typically).
(%:>) :: (Monad m, MonadPlus f) => ParserT m s a -> ParserT m s (f a) -> ParserT m s (f a)
p1 %:> p2 = (fmap pure p1) %> p2

-- | Parser that succeeds if the head of the stream satisfies the given predicate. If it does not or if
-- the stream is done, it fails. This returns the consumed symbol.
parseP :: (Alternative m, Stream s) => (a -> Bool) -> ParserT m (s a) a
parseP p = ParserT $ \s ->
    case uncons s of
      Just (x, xs) | p x -> pure (xs, x)
      _ -> empty

-- | Same as `parserP` but ignores the parsed symbol (and returns `()`).
parseP_ :: (Alternative m, Stream s) => (a -> Bool) -> ParserT m (s a) ()
parseP_ p = ParserT $ \s ->
    case uncons s of
      Just (x, xs) | p x -> pure (xs, ())
      _ -> empty

-- | Parser that succeeds if the head of the stream is the given symbol, and fails otherwise or if the
-- stream is done.
parseChar :: (Alternative m, Stream s, Eq a) => a -> ParserT m (s a) a
parseChar c = parseP (== c)

-- | Same as `parseChar` but drops the symbol and returns `()`.
parseChar_ :: (Alternative m, Stream s, Eq a) => a -> ParserT m (s a) ()
parseChar_ c = parseP_ (== c)

-- | Parser that succeeds if the first elements of the stream correspond to the given sequence of
-- symbols. If the stream is empty (or not long enough) or if the sequence is not right, it fails.
--
-- The returned result correspond to each character of the sequence that was parsed successfully, wrapped
-- in the `MonadPlus` result.
parseSeq :: (Monad m, Alternative m, Eq a, Foldable t, Stream s, MonadPlus f) => t a -> ParserT m (s a) (f a)
parseSeq = foldl (\acc x -> acc %> (pure <$> parseChar x)) zero

-- | Same as `parseSeq` but drops the result and only return `()`.
parseSeq_ :: (Monad m, Alternative m, Eq a, Foldable t, Stream s) => t a -> ParserT m (s a) ()
parseSeq_ = foldl (\acc x -> acc >> parseP_ (== x)) (return ())

-- | Parse exactly one blank space (and drop it).
parseSpace :: (Alternative m, Stream s) => ParserT m (s Char) ()
parseSpace = parseP_ (Char.isSpace)

-- | Parse a sequence of blank spaces (at least one) and drop the result.
parseSpaces :: (Monad m, Alternative m, Stream s) => ParserT m (s Char) ()
parseSpaces = parseSpace >> (parseSpaces <|> return ())

-- | Parse one digit and return the corresponding `Int`.
parseDigit :: (Alternative m, Stream s) => ParserT m (s Char) Int
parseDigit = Char.digitToInt <$> (parseP Char.isDigit)

-- | Parse a full number (and return its value). A number in this parser is simply a sequence of
-- digits with no further constraints. I do not think it works on hexadecimal numbers (?).
parseNumber :: (Monad m, Alternative m, Stream s) => ParserT m (s Char) Int
parseNumber =
    parseDigit >>= parsenum
    where parsenum x = (parseDigit >>= \y -> parsenum (10 * x + y)) <|> return x

-- | Parse a type based on its textual representation given by its `Show` typeclass intsance.
parseShow :: (Monad m, Alternative m, Show x, Stream s) => x -> ParserT m (s Char) x
parseShow x = parseSeq_ (show x) >> return x



