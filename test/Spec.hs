module Main where

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import AbstractSyntaxTree (eval, run)

-- | Validate the parser by going through all allowed and diallowed
-- cases.
main :: IO ()
main =
  hspec $
  describe "AbstractSyntaxTree.eval" $ do
    it "can evaluate addition" $
      property $ \(n, m) -> do
        let res = eval $ run $ show n ++ "+" ++ show m
        res == (n + m :: Int)
    it "can evaluate subtraction" $
      property $ \(n, m) -> do
        let res = eval $ run $ show n ++ "-" ++ show m
        res == (n - m :: Int)
    it "can evaluate multiplication" $
      property $ \(n, m) -> do
        let res = eval $ run $ show n ++ "*" ++ show m
        res == (n * m :: Int)
    it "can evaluate parentheses" $
      property $ \(n, m) -> do
        let res =
              eval $
              run $
              show n ++ "+" ++ show m ++ "+(" ++ show n ++ "+" ++ show m ++ ")"
        res == (n + m + (n + m) :: Int)
    it "cannot evaluate division" $
      evaluate (eval $ run "5/8") `shouldThrow` anyException
    it "cannot parse spaces" $
      evaluate (eval $ run "5 +8") `shouldThrow` anyException
    it "cannot evaluate characters" $
      property $ \(n, m) -> do
        let res = eval $ run $ show (n :: Char) ++ "+" ++ show (m :: Char)
        evaluate res `shouldThrow` anyException
