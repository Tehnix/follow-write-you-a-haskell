{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleInstances #-}

module NanoParsec where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List

-- | Our `Parser a` type denotes that we are taking in a `String` and then
-- parsing it into a value, `a` (usually the AST), keeping the remaning input
-- stream in the `String` in `(a, String)`.
newtype Parser s a = Parser
  { parse :: s -> [(a, s)]
  }

-- | A simple parser which returns the result, `res`, if the entire stream was
-- successfully consumed, else an error condition.
runParser :: (Parseable p) => Parser p a -> p -> a
runParser m s =
  case parse m s of
    [(res, rs)] ->
      if nullP rs
        then res
        else error "Parser did not consume entire stream"
    _ -> error "Parser error."

class (Eq p) =>
      Parseable p where
  emptyP :: p
  headP :: p -> p
  tailP :: p -> p
  nullP :: p -> Bool
  appendP :: String -> p -> String
  elemP :: p -> p -> Bool
  isDigitP :: p -> Bool

instance Parseable [Char] where
  emptyP = []
  headP (a:_) = [a]
  tailP (_:as) = as
  nullP [] = True
  nullP _ = False
  appendP s p = s ++ p
  elemP c s = c `isInfixOf` s
  isDigitP s = isDigit (head s)

-- | The functor for our `Parser` applies the function, `f`, to all values, `a`,
-- in the result tuple.
instance (Parseable s) => Functor (Parser s) where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

-- | Our applicative takes the value of the first `Parser`, `f`, and applies it
-- onto the value of the second parser, `a`, while only keeping the remaning
-- streams of the second parser. For `pure`, c.f. the documentation for `unit`.
instance (Parseable s) => Applicative (Parser s) where
  pure = unit
  (Parser cs1) <*> (Parser cs2) =
    Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

-- | Monad simply conisists of `bind` and `pure` from our `Applicative` instance
-- (which is `unit`).
instance (Parseable s) => Monad (Parser s) where
  (>>=) = bind
  return = pure

-- | Since we have a monoidal structure, we can define an `Alternative` for
-- our `Parser`, which defines an empty structure and a way to combine two
-- optional paths of parser logic. We automatically get `some` and `many` from
-- this instance.
instance (Parseable s) => Alternative (Parser s) where
  empty = failure
  (<|>) = option

-- | Our monad has a zero value, `empty`, (when we halt and return the stream),
-- and a way to combine, `combine`. This together with the `Alternative` instance
-- gives us a way to encode logic for trying multiple parse functions over the
-- same stream, with failure and rollover handling.
instance (Parseable s) => MonadPlus (Parser s) where
  mzero = empty
  mplus = combine

-- | Extract a single character from the parser stream and return a tuple
-- containing the character, `c`, and the rest of the stream, `cs`.
item :: (Parseable p) => Parser p p
item =
  Parser $ \s ->
    if nullP s
      then []
      else [(headP s, tailP s)]

-- | Take one parse operation, `Parser a`, and compose it over the result of a
-- second parse function, `(a -> Parser s b)`. Composing with the a second parser
-- means mapping itself over the first parsers list of tuples and concatenating
-- the resulting nested list of lists into a single flat list.
bind :: Parser s a -> (a -> Parser s b) -> Parser s b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

-- | `unit` simply injects a single pure value, `a`, without reading anything
-- from the parse stream, `s`.
unit :: a -> Parser s a
unit a = Parser (\s -> [(a, s)])

-- | Halt and return the stream with an empty list.
failure :: Parser s a
failure = Parser (\cs -> [])

-- | Combine two parsers together by applying them on the same stream
combine :: Parser s a -> Parser s a -> Parser s a
combine p q = Parser (\s -> parse p s ++ parse q s)

-- | Run a parser on the input stream, and if it fails (an empty list, c.f.
-- `failure`), then try the second parser, else return the result of the first.
option :: Parser s a -> Parser s a -> Parser s a
option p q =
  Parser $ \s ->
    case parse p s of
      [] -> parse q s
      res -> res

-- | Check if the current character in the stream matches a given predicate, `p`.
satisfy :: (Parseable p) => (p -> Bool) -> Parser p p
satisfy p =
  item `bind` \c ->
    if p c
      then unit c
      else Parser (\cs -> [])

-- | Check if any one of the characters, `s`, occur.
oneOf :: (Parseable p) => p -> Parser p p
oneOf s = satisfy (flip elemP s)

-- | Try `chainl1`, returning the original element on failure.
chainl :: Parser s a -> Parser s (a -> a -> a) -> a -> Parser s a
chainl p op a = (p `chainl1` op) <|> return a

-- | Parse one of more occurences of `p`, separated by `op` and return a value
-- obtained by recursing until failure on the left hand side of the stream.
-- NOTE: Can be used to parse left-recursive grammars.
chainl1 :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
p `chainl1` op = do
  a <- p
  rest a
  where
    rest a =
      (do f <- op
          b <- p
          rest (f a b)) <|>
      return a

-- | Parse the item if it is a character.
char :: (Parseable p) => p -> Parser p p
char c = satisfy (c ==)

-- | Parse the item if it is a natural number.
natural :: (Parseable p) => Parser p Integer
natural = read <$> some (satisfy isDigitP)

-- | Recursively check if the input stream is a string by concatenating a list
-- of characters.
string :: String -> Parser s String
string [] = return []
string (c:cs) = do
  char c
  string cs
  return (c : cs)

-- | Parse the items if they contain any space character (including newlines).
spaces :: Parser s [String]
spaces = many $ oneOf " \n\r"

-- | Run a parser on the item and then consume the subsequent spaces, returning
-- the result after.
token :: Parser s a -> Parser s a
token p = do
  a <- p
  spaces
  return a

-- | Parse the item if it is a reserved word (i.e. matches the given string).
reserved :: String -> Parser s String
reserved s = token (string s)

-- | Parse the item if it is a digit.
digit :: Parser s Char
digit = satisfy isDigitP

-- | Parse the item if it is a number, including support for unary minus.
number :: Parser s Int
number = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read (s ++ cs)

-- | Run a parser in between two parentheses, returning only the result of the
-- parser.
parens :: Parser s a -> Parser s a
parens m = do
  reserved "("
  n <- m
  reserved ")"
  return n
