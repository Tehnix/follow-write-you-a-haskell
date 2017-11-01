module Repl
  ( process
  , repl
  ) where

import Eval
import Parser
import Pretty

import Control.Monad.Trans
import System.Console.Haskeline

-- | Take in the repl input, parse it, evaluate it and then return the result.
-- NOTE: We return a `String` instead of the original `IO ()`, so that we can test
-- the function more easily.
process :: String -> String
process line = do
  let res = parseExpr line
  case res of
    Left err -> show err
    Right ex ->
      case eval ex of
        Nothing -> "Cannot evaluate"
        Just result -> ppexpr result

-- | Run an infinite loop, taking in the users input and evaluating it.
repl :: IO ()
repl = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "Repl> "
      case minput of
        Nothing -> outputStrLn "Goodby."
        Just input -> (liftIO $ print (process input)) >> loop
