module Eval where

import Syntax

import Data.Functor
import Data.Maybe

-- | Check if value is a number.
isNum :: Expr -> Bool
isNum Zero = True
isNum (Succ t) = isNum t
isNum _ = False

-- | Check if we are dealing with a value.
isVal :: Expr -> Bool
isVal Tr = True
isVal Fl = True
isVal t
  | isNum t = True
isVal _ = False

-- | Perform the evaluation by taking a single small step from one expression
-- to another given the rules in the case. We use the `Maybe` applicative to 
-- handle the fact that our reduction may halt at any time, with a `Nothing`,
-- or simply isn't well defined.
-- Simplified: Apply `eval'` on the expression until we hit a literal value.
eval' :: Expr -> Maybe Expr
eval' x =
  case x of
    IsZero Zero -> Just Tr
    IsZero (Succ t)
      | isNum t -> Just Fl
    IsZero t -> IsZero <$> (eval' t)
    Succ t -> Succ <$> (eval' t)
    Pred Zero -> Just Zero
    Pred (Succ t)
      | isNum t -> Just t
    Pred t -> Pred <$> (eval' t)
    If Tr c _ -> Just c
    If Fl _ a -> Just a
    If t c a -> (\t' -> If t' c a) <$> eval' t
    _ -> Nothing

-- | Return the evaluated expression, if successfull, or the original expression
-- back if not. 
nf :: Expr -> Expr
nf x = fromMaybe x (nf <$> eval' x)

-- | Evaluate the expression, returning the result if it's a value, otherwise
-- `Nothing` if the term evaluation is stuck.
eval :: Expr -> Maybe Expr
eval t =
  case nf t of
    nft
      | isVal nft -> Just nft
      | otherwise -> Nothing -- the term is "stuck"
