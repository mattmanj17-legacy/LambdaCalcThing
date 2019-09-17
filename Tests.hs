{-# OPTIONS_GHC -Wall #-}

import Test.Hspec

import ParseCommonTests
import DebrujinTests
import LambdaTests
import ParseLambdaLikeTests
import LambdaToDebrujinTests
import DebrujinToLambdaTests
import ReduceDebrujinTests

main :: IO ()
main = hspec $ do
  describe "lambda" $ do
    parseCommonTests
    parseLambdaLikeTests
    debrujinTests
    lambdaTests
    lambdaToDebrujinTests
    debrujinToLambdaTests
    reduceDebrujinTests