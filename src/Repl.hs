module Repl
  ( process
  , repl
  ) where

import Eval
import Parser
import Pretty
import Syntax

import Control.Monad.Trans
import System.Console.Haskeline

showStep :: (Int, Expr) -> IO ()
showStep (d, x) = putStrLn ((replicate d ' ') ++ "=> " ++ ppexpr x)

-- | Take in the repl input, parse it, evaluate it and then return the result.
-- NOTE: We return a `String` instead of the original `IO ()`, so that we can test
-- the function more easily.
process :: String -> IO ()
process line = do
  let res = parseExpr line
  case res of
    Left err -> print err
    Right ex -> do
      let (out, ~steps) = runEval ex
      mapM_ showStep steps
      print out

-- | Run an infinite loop, taking in the users input and evaluating it.
repl :: IO ()
repl = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "Untyped> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> (liftIO $ process input) >> loop
