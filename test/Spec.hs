module Main where

import Control.Exception (evaluate)
import Control.Monad.Trans
import Test.Hspec
import Test.QuickCheck

import Repl (process)

-- | Validate the parser by going through all allowed and diallowed
-- cases.
main :: IO ()
main =
  hspec $
  describe "Repl.repl" $ do
    it "succ 0 == id" $ do
      let line = "succ 0"
      (process line) `shouldBe` line
    it "nested succ 0's == id" $ do
      let line = "succ (succ 0)"
      (process line) `shouldBe` line
    it "iszero 0 is true" $ do
      let line = "iszero 0"
      (process line) `shouldBe` "true"
    it "iszero (succ 0) is false" $ do
      let line = "iszero (succ 0)"
      (process line) `shouldBe` "false"
    it "pred (succ 0) is 0" $ do
      let line = "pred (succ 0)"
      (process line) `shouldBe` "0"
    it "iszero (pred (succ (succ 0))) is false" $ do
      let line = "iszero (pred (succ (succ 0)))"
      (process line) `shouldBe` "false"
    it "if false then true else false evaluates to false" $ do
      let line = "if false then true else false"
      (process line) `shouldBe` "false"
    it "if 0 then true else false cannot evaluate" $ do
      let line = "if 0 then true else false"
      (process line) `shouldBe` "Cannot evaluate"
    it "iszero false cannot evaluate" $ do
      let line = "iszero false"
      (process line) `shouldBe` "Cannot evaluate"
