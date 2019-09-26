
module UnAnonLambdaTests 
(
  unAnonLambdaTests
)
where

import Data.Either

import Test.Hspec

import LambdaAst
import UnAnonLambda
import TestHelpers
import ParseCommon
import ParseLambda
import LambdaTestHelpers

unAnonLambdaTest :: String -> LambdaAst -> LambdaAst -> SpecWith ()
unAnonLambdaTest strDesc lambdaIn lambdaOut = do
  it strDesc $ do
    unanoned `shouldSatisfy` isEsRight
  if isEsRight unanoned then
    it strDesc $ do
      cmpLambdaForTest justUnanoned lambdaOut `shouldBe` True
  else
    return ()
  where
    unanoned = unAnonLambda lambdaIn
    justUnanoned = fromEsRightUnsafe unanoned

maybeLambdaToLambdaTest :: String -> Either String LambdaAst -> LambdaAst -> SpecWith ()
maybeLambdaToLambdaTest strDesc = runBinaryTestWithMaybeInput strDesc unAnonLambdaTest

lambdaStrToLambdaTest :: String -> String -> LambdaAst -> SpecWith ()
lambdaStrToLambdaTest strDesc strIn out = do
  let parsedIn = parseFromStrToEither parseLambda strIn
  maybeLambdaToLambdaTest strDesc parsedIn out

lambdaStrToLambdaStrTest :: String -> String -> String -> SpecWith ()
lambdaStrToLambdaStrTest strDesc strIn strOut = do
  let out = parseFromStrToEither parseLambda strOut
  it strDesc $ do
    out `shouldSatisfy` isRight
  either (const $ return ()) (lambdaStrToLambdaTest strDesc strIn) out

unAnonLambdaTests = do
  unAnonLambdaTest "ual0"
    (LambdaArgRef 1)
    (LambdaArgRef 1)
  lambdaStrToLambdaStrTest "ual1"
    "(% #1)"
    "(/ [a] a)"
  lambdaStrToLambdaStrTest "ual2"
    "(% (% (#1 #2)))"
    "(/ [a] (/ [b] (b a)))"
  lambdaStrToLambdaStrTest "ual3"
    "(% (% #1))"
    "(/ [a] (/ [b] b))"
  lambdaStrToLambdaStrTest "ual4"
    "((% #1) (%#1))"
    "((/ [a] a) (/ [a] a))"
  lambdaStrToLambdaStrTest "ual5"
    "((% (% #2)) (% #1))"
    "((/ [a] (/ [b] a)) (/ [a] a))"
  lambdaStrToLambdaStrTest "ual6"
    "(% (% (% (% ((#4 #2) ((#3 #2) #1))))))"
    "(/ [a] (/ [b] (/ [c] (/ [d] ((a c) ((b c) d))))))"
  lambdaStrToLambdaStrTest "ual7"
    "(% (% #1))"
    "(/ [a] (/ [b] b))"
  lambdaStrToLambdaStrTest "ual8"
    "(% (% (#2 #1)))"
    "(/ [a] (/ [b] (a b)))"
  lambdaStrToLambdaStrTest "ual9"
    "(% (% (#2 (#2 #1))))"
    "(/ [a] (/ [b] (a (a b))))"
  lambdaStrToLambdaStrTest "ual10"
    "(% (% (#2 (#2 (#2 #1)))))"
    "(/ [a] (/ [b] (a (a (a b)))))"
  lambdaStrToLambdaStrTest "ual11"
    "(/ [a] a)"
    "(/ [a] a)"
  lambdaStrToLambdaStrTest "ual12"
    "(% (/ [a] (a #1 #2)))"
    "(/ [a] (/ [b] (b b a)))"
  lambdaStrToLambdaStrTest "ual13"
    "(/ [a] (% (a #1 #2)))"
    "(/ [a] (/ [b] (a b a)))"
  lambdaStrToLambdaStrTest "ual14"
    "(/ [a] (% (% (a #1 #2))))"
    "(/ [a] (/ [b] (/ [c] (a c b))))"