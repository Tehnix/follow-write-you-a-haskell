{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module NanoParsec where

import Control.Applicative
import Control.Monad
import Data.Char

-- | Our `Parser a` type denotes that we are taking in a `String` and then
-- parsing it into a value, `a` (usually the AST), keeping the remaning input
-- stream in the `String` in `(a, String)`.
newtype Parser a = Parser
  { parse :: String -> [(a, String)]
  }

-- | A simple parser which returns the result, `res`, if the entire stream was
-- successfully consumed, else an error condition.
runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, rs)]   -> error $ "Parser did not consume entire stream: " ++ rs
    _           -> error "Parser error."

-- | The functor for our `Parser` applies the function, `f`, to all values, `a`,
-- in the result tuple.
instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

-- | Our applicative takes the value of the first `Parser`, `f`, and applies it
-- onto the value of the second parser, `a`, while only keeping the remaning
-- streams of the second parser. For `pure`, c.f. the documentation for `unit`.
instance Applicative Parser where
  pure = unit
  (Parser cs1) <*> (Parser cs2) =
    Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

-- | Monad simply conisists of `bind` and `pure` from our `Applicative` instance
-- (which is `unit`).
instance Monad Parser where
  (>>=) = bind
  return = pure

-- | Since we have a monoidal structure, we can define an `Alternative` for
-- our `Parser`, which defines an empty structure and a way to combine two
-- optional paths of parser logic. We automatically get `some` and `many` from
-- this instance.
instance Alternative Parser where
  empty = failure
  (<|>) = option

-- | Our monad has a zero value, `empty`, (when we halt and return the stream),
-- and a way to combine, `combine`. This together with the `Alternative` instance
-- gives us a way to encode logic for trying multiple parse functions over the
-- same stream, with failure and rollover handling.
instance MonadPlus Parser where
  mzero = empty
  mplus = combine

-- | Extract a single character from the parser stream and return a tuple
-- containing the character, `c`, and the rest of the stream, `cs`.
item :: Parser Char
item =
  Parser $ \s ->
    case s of
      []     -> []
      (c:cs) -> [(c, cs)]

-- | Take one parse operation, `Parser a`, and compose it over the result of a
-- second parse function, `(a -> Parser b)`. Composing with the a second parser
-- means mapping itself over the first parsers list of tuples and concatenating
-- the resulting nested list of lists into a single flat list.
bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

-- | `unit` simply injects a single pure value, `a`, without reading anything
-- from the parse stream, `s`.
unit :: a -> Parser a
unit a = Parser (\s -> [(a, s)])

-- | Halt and return the stream with an empty list.
failure :: Parser a
failure = Parser (\cs -> [])

-- | Combine two parsers together by applying them on the same stream
combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

-- | Run a parser on the input stream, and if it fails (an empty list, c.f.
-- `failure`), then try the second parser, else return the result of the first.
option :: Parser a -> Parser a -> Parser a
option p q =
  Parser $ \s ->
    case parse p s of
      []  -> parse q s
      res -> res

-- | Check if the current character in the stream matches a given predicate, `p`.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p =
  item `bind` \c ->
    if p c
      then unit c
      else Parser (\cs -> [])

-- | Check if any one of the characters, `s`, occur.
oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

-- | Try `chainl1`, returning the original element on failure.
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

-- | Parse one of more occurences of `p`, separated by `op` and return a value
-- obtained by recursing until failure on the left hand side of the stream.
-- NOTE: Can be used to parse left-recursive grammars.
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
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
char :: Char -> Parser Char
char c = satisfy (c ==)

-- | Parse the item if it is a natural number.
natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

-- | Recursively check if the input stream is a string by concatenating a list
-- of characters.
string :: String -> Parser String
string [] = return []
string (c:cs) = do
  char c
  string cs
  return (c : cs)

-- | Parse the items if they contain any space character (including newlines).
spaces :: Parser String
spaces = many $ oneOf " \n\r"

-- | Run a parser on the item and then consume the subsequent spaces, returning
-- the result after.
token :: Parser a -> Parser a
token p = do
  a <- p
  spaces
  return a

-- | Parse the item if it is a reserved word (i.e. matches the given string).
reserved :: String -> Parser String
reserved s = token (string s)

-- | Parse the item if it is a digit.
digit :: Parser Char
digit = satisfy isDigit

-- | Parse the item if it is a number, including support for unary minus.
number :: Parser Int
number = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read (s ++ cs)

-- | Run a parser in between two parentheses, returning only the result of the
-- parser.
parens :: Parser a -> Parser a
parens m = do
  reserved "("
  n <- m
  reserved ")"
  return n
