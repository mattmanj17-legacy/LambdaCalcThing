
module AnonLambdaTests
(
  anonLambdaTests
)
where

import Data.Maybe
import Test.Hspec

import ParseCommon
import ParseLambda
import LambdaAst
import ReduceLambda
import TestHelpers

anonTest :: String -> LambdaAst -> LambdaAst -> SpecWith ()
anonTest strDesc lambdaIn lambdaOut = do
  it strDesc $ do
    anoned `shouldSatisfy` isJust
  if isJust anoned then
    it strDesc $ do
      justAnoned `shouldBe` lambdaOut
  else
    return ()
  where
    anoned = anonLambda lambdaIn
    justAnoned = fromJust anoned

anonMaybeLambdaToLambdaTest :: String -> Maybe LambdaAst -> LambdaAst -> SpecWith ()
anonMaybeLambdaToLambdaTest strDesc = runBinaryTestWithMaybeInput strDesc anonTest

anonLambdaStrToLambdaTest :: String -> String -> LambdaAst -> SpecWith ()
anonLambdaStrToLambdaTest strDesc strIn out = do
  let parsedIn = parseFromStrToMaybe parseLambda strIn
  anonMaybeLambdaToLambdaTest strDesc parsedIn out

anonLambdaStrToStrTest :: String -> String -> String -> SpecWith ()
anonLambdaStrToStrTest strDesc strIn strOut = do
  let out = parseFromStrToMaybe parseLambda strOut
  it strDesc $ do
    out `shouldSatisfy` isJust
  maybe (return ()) (anonLambdaStrToLambdaTest strDesc strIn) out

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
    (anonLambda (fromJust (parseFromStrToMaybe parseLambda "(/ [x] (/ [x] x))"))) `shouldBe` Nothing
  it "al8" $ do
    (anonLambda (fromJust (parseFromStrToMaybe parseLambda "(/ [x x] x)"))) `shouldBe` Nothing
  it "al9" $ do
    (anonLambda (fromJust (parseFromStrToMaybe parseLambda "(/ [a b c] (/ [c d e] e))"))) `shouldBe` Nothing
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