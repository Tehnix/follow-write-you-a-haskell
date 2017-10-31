{-
Our grammar, which is translated into Haskell, given in Backus-Naur Form (BNF):

number = [ "-" ] digit { digit }.
digit  = "0" | "1" | ... | "8" | "9".
expr   = term { addop term }.
term   = factor { mulop factor }.
factor = "(" expr ")" | number.
addop  = "+" | "-".
mulop  = "*".

-}
module AbstractSyntaxTree where

import Control.Applicative
import NanoParsec

-- | Our simple abstract syntax tree (AST), supporting binary operations and
-- integer literals.
data Expr
  = Add Expr
        Expr
  | Mul Expr
        Expr
  | Sub Expr
        Expr
  | Lit Int
  deriving (Show)

-- | Recursively evaluate an expression until it hits a literal, and then reduce
-- it, applying the operators encountered in the AST.
eval :: Expr -> Int
eval ex =
  case ex of
    Add a b -> eval a + eval b
    Mul a b -> eval a * eval b
    Sub a b -> eval a - eval b
    Lit n   -> n

-- | Parse in one or more occurences of a `term`, separated by an operator,
-- `addop` (addition and subtraction).
expr :: Parser Expr
expr = term `chainl1` addop

-- | Parse in one or more occurences of a `factor`, separated by an operator,
-- `mulop` (only multiplication).
term :: Parser Expr
term = factor `chainl1` mulop

-- | Parse in an integer or an expression surrounded by parentheses.
factor :: Parser Expr
factor = int <|> parens expr

-- | Parse in a number to a literal.
int :: Parser Expr
int = do
  n <- number
  return (Lit n)

-- | Consume the operator, `x`, and return the expression function, `f`.
infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reserved x >> return f

-- | Parse either a plus or a minus.
addop :: Parser (Expr -> Expr -> Expr)
addop = (infixOp "+" Add) <|> (infixOp "-" Sub)

mulop :: Parser (Expr -> Expr -> Expr)
mulop = infixOp "*" Mul

run :: String -> Expr
run = runParser expr
