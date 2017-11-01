module Syntax where

-- | Our abstract syntax tree for our language grammar.
data Expr
  = Tr
  | Fl
  | Zero
  | IsZero Expr
  | Succ Expr
  | Pred Expr
  | If Expr
       Expr
       Expr
  deriving (Eq, Show)
