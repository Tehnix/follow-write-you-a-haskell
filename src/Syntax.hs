module Syntax where

type Name = String

-- | Our abstract syntax tree for our language grammar.
data Expr
  = Var Name
  | Lit Lit
  | App Expr
        Expr
  | Lam Name
        Expr
  deriving (Eq, Show)

data Lit
  = LInt Int
  | LBool Bool
  deriving (Show, Eq, Ord)
