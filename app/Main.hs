module Main where

import AbstractSyntaxTree (eval, run)

import Control.Monad

main :: IO ()
main =
  forever $ do
    putStr "> "
    a <- getLine
    print $ eval $ run a
