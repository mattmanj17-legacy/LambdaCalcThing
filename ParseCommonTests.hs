
module ParseCommonTests 
(
  parseCommonTests
)
where

import Data.Either

import Test.Hspec
import Text.ParserCombinators.Parsec

import ParseCommon

parseCommonTests = do
  it "parseCommonTests" $ do
    False `shouldBe` False -- nothing interesting yet