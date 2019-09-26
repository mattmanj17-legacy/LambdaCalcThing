
module AnonLambdaTests
(
  anonLambdaTests
)
where

import Data.Either
import Test.Hspec

import ParseCommon
import ParseLambda
import LambdaAst
import ReduceLambda
import TestHelpers
import LambdaTestHelpers

anonTest :: String -> LambdaAst -> LambdaAst -> SpecWith ()
anonTest strDesc lambdaIn lambdaOut = do
  it strDesc $ do
    anoned `shouldSatisfy` isEsRight
  if isEsRight anoned then
    it strDesc $ do
      cmpLambdaForTest justAnoned lambdaOut `shouldBe` True
  else
    return ()
  where
    anoned = anonLambda lambdaIn
    justAnoned = fromEsRightUnsafe anoned

anonMaybeLambdaToLambdaTest :: String -> Either String LambdaAst -> LambdaAst -> SpecWith ()
anonMaybeLambdaToLambdaTest strDesc = runBinaryTestWithMaybeInput strDesc anonTest

anonLambdaStrToLambdaTest :: String -> String -> LambdaAst -> SpecWith ()
anonLambdaStrToLambdaTest strDesc strIn out = do
  let parsedIn = parseFromStrToEither parseLambda strIn
  anonMaybeLambdaToLambdaTest strDesc parsedIn out

anonLambdaStrToStrTest :: String -> String -> String -> SpecWith ()
anonLambdaStrToStrTest strDesc strIn strOut = do
  let out = parseFromStrToEither parseLambda strOut
  it strDesc $ do
    out `shouldSatisfy` isRight
  either (const $ return ()) (anonLambdaStrToLambdaTest strDesc strIn) out

anonLambdaTests = do
  anonLambdaStrToStrTest "al0"
    "foo"
    "foo"
  anonLambdaStrToStrTest "al1"
    "(/ [x] y)"
    "(% y)"
  anonLambdaStrToStrTest "al2"
    "(/ [x z] y)"
    "(% (% y))"
  anonLambdaStrToStrTest "al3"
    "(/ [x y] y)"
    "(% (% #1))"
  anonLambdaStrToStrTest "al4"
    "(/ [y x] y)"
    "(% (% #2))"
  anonLambdaStrToStrTest "al5"
    "(x y)"
    "(x y)"
  anonLambdaStrToStrTest "al6"
    "(/ [x] x)"
    "(% #1)"
  it "al7" $ do
    (anonLambda (fromRightUnsafe (parseFromStrToEither parseLambda "(/ [x] (/ [x] x))"))) `shouldNotSatisfy` isEsRight
  it "al8" $ do
    (anonLambda (fromRightUnsafe (parseFromStrToEither parseLambda "(/ [x x] x)"))) `shouldNotSatisfy` isEsRight
  it "al9" $ do
    (anonLambda (fromRightUnsafe (parseFromStrToEither parseLambda "(/ [a b c] (/ [c d e] e))"))) `shouldNotSatisfy` isEsRight
  anonLambdaStrToStrTest "al10"
    "(/ [y] (/ [x] (x y)))"
    "(% (% (#1 #2)))"
  anonLambdaStrToStrTest "al11"
    "(/ [y] (/ [x] (x y x y)))"
    "(% (% (#1 #2 #1 #2)))"
  anonLambdaStrToStrTest "al12"
    "(/ [y] (/ [x] x))"
    "(% (% #1))"
  anonLambdaStrToStrTest "al13"
    "((/ [x] x) (/ [x] x))"
    "((% #1) (% #1))"
  anonLambdaStrToStrTest "al14"
    "((/ [x] (/ [y] x)) (/ [x] x))"
    "((% (% #2)) (% #1))"
  anonLambdaStrToStrTest "al15"
    "(/ [m] (/ [n] (/ [f] (/ [x] ((m f) ((n f) x))))))"
    "(% (% (% (% ((#4 #2) ((#3 #2) #1))))))"
  anonLambdaStrToStrTest "al16"
    "(/ [m n f x] (m f (n f x)))"
    "(% (% (% (% (#4 #2 (#3 #2 #1))))))"
  anonLambdaStrToStrTest "al17"
    "(/ [f] (/ [x] x))"
    "(% (% #1))"
  anonLambdaStrToStrTest "al18"
    "(/ [f] (/ [x] (f x)))"
    "(% (% (#2 #1)))"
  anonLambdaStrToStrTest "al19"
    "(/ [f] (/ [x] (f (f x))))"
    "(% (% (#2 (#2 #1))))"
  anonLambdaStrToStrTest "al20"
    "(/ [f] (/ [x] (f (f (f x)))))"
    "(% (% (#2 (#2 (#2 #1)))))"