module ParserSpec (spec) where

import Test.Hspec
import Test.HUnit
import Test.QuickCheck
import Control.Exception
import Control.Monad.Trans.State
import Data.Char
import Data.Either
import Parser


spec :: Spec
spec = do 
  describe "char" $ do
    it "works to spec" $ do
      parse char "a" `shouldBe` Right ('a', "")
      parse char " foo " `shouldBe` Right (' ', "foo ")
      parse char "123" `shouldBe` Right ('1', "23")
      parse char "" `shouldSatisfy` isLeft

  describe "sat" $ do
    it "works to spec" $ do
      parse (sat isAlpha) "a" `shouldBe` Right ('a', "")
      parse (sat isDigit) "1" `shouldBe` Right ('1', "")
      parse (sat isAlpha) "123" `shouldSatisfy` isLeft
      parse (sat isDigit) "abc" `shouldSatisfy` isLeft

  describe "oneOf" $ do
    it "works to spec" $ do
      parse (oneOf ["a", "b", "c"]) "a 2 3" `shouldBe` Right ("a", "2 3")
      parse (oneOf ["a", "b", "c"]) "bac" `shouldBe` Right ("b", "ac")
      parse (oneOf ["+", "*", "=="]) "== 10" `shouldBe` Right ("==", "10")
      parse (oneOf ["a", "b", "c"]) "d" `shouldSatisfy` isLeft
      
  describe "identifier" $ do
    it "works to spec" $ do
      parse identifier "a" `shouldBe` Right ("a", "")
      parse identifier " foo " `shouldBe` Right ("foo", "")
      parse identifier "aB2c3 10" `shouldBe` Right ("aB2c3", "10")
      parse identifier "foo123 = x" `shouldBe` Right ("foo123", "= x")
      parse identifier "123" `shouldSatisfy` isLeft
      parse identifier "   " `shouldSatisfy` isLeft
