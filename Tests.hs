{-# OPTIONS_GHC -Wall #-}

import Test.Hspec

import ParseCommonTests
import DebrujinTests
import LambdaTests
import ParseLambdaLikeTests
import LambdaToDebrujinTests
import ReduceDebrujinTests

main :: IO ()
main = hspec $ do
  describe "lambda" $ do
    parseCommonTests
    debrujinTests
    lambdaTests
    parseLambdaLikeTests
    lambdaToDebrujinTests
    reduceDebrujinTests