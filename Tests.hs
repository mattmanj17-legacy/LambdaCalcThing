{-# OPTIONS_GHC -Wall #-}

import Test.Hspec

import LambdaAstTests
import ParseLambdaTests
import AnonLambdaTests
import UnAnonLambdaTests
import ReduceLambdaTests

main :: IO ()
main = hspec $ do
  describe "lambda" $ do
    parseLambdaTests
    lambdaAstTests
    anonLambdaTests
    unAnonLambdaTests
    reduceLambdaTests