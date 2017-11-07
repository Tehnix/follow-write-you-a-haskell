{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Pretty
  ( ppexpr
  ) where

import Syntax

import Text.PrettyPrint (Doc, (<+>), (<>))
import qualified Text.PrettyPrint as PP

-- | Insert a parenthese if the predicate is true, else simply return the item.
parensIf :: Bool -> Doc -> Doc
parensIf True = PP.parens
parensIf False = id

class Pretty p where
  ppr :: Int -> p -> Doc

instance Pretty Name where
  ppr _ x = PP.text x

instance Pretty Expr where
  ppr _ (Var x) = PP.text x
  ppr _ (Lit (LInt a)) = PP.text (show a)
  ppr _ (Lit (LBool b)) = PP.text (show b)
  ppr p e@(App _ _) =
    parensIf (p > 0) (ppr p f <+> PP.sep (map (ppr (p + 1)) xs))
    where
      (f, xs) = viewApp e
  ppr p e@(Lam _ _) =
    parensIf (p > 0) $ PP.char '\\' <> PP.hsep vars <+> PP.text "." <+> body
    where
      vars = map (ppr 0) (viewVars e)
      body = ppr (p + 1) (viewBody e)

-- | Collapse lambda variables, so we can print them as a single expression.
viewVars :: Expr -> [Name]
viewVars (Lam n a) = n : viewVars a
viewVars _ = []

-- | Collapse lambda bodies, so we can print them as a single expression.
viewBody :: Expr -> Expr
viewBody (Lam _ a) = viewBody a
viewBody x = x

-- | Split up the bodies and vars of a lambda application.
viewApp :: Expr -> (Expr, [Expr])
viewApp (App e1 e2) = go e1 [e2]
  where
    go (App a b) xs = go a (b : xs)
    go f xs = (f, xs)
viewApp _ = error "not application"

-- | Render the pretty printer output.
ppexpr :: Expr -> String
ppexpr = PP.render . ppr 0
