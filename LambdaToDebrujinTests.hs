
module LambdaToDebrujinTests
(
  lambdaToDebrujinTests
)
where

import Data.Maybe
import Test.Hspec

import ParseCommon
import ParseLambdaLike
import Lambda
import Debrujin
import LambdaToDebrujin
import TestHelpers

anonTest :: Lambda -> Maybe Debrujin -> SpecWith ()
anonTest lambda debrujin =
   it ("anonTest " ++ show lambda ++ " " ++ show debrujin) $ do
    (lambdaBoundVarsAnonymized lambda) `shouldBe` debrujin

maybeLambdaToDebrujinTest :: Maybe Lambda -> Maybe Debrujin -> SpecWith ()
maybeLambdaToDebrujinTest = runBinaryTestWithMaybeInput anonTest

lambdaStrToDebrujinTest :: String -> Maybe Debrujin -> SpecWith ()
lambdaStrToDebrujinTest strIn out = do
  let parsedIn = parseFromStrToMaybe parseLambda strIn
  maybeLambdaToDebrujinTest parsedIn out

lambdaStrToDebrujinStrTest :: String -> String -> SpecWith ()
lambdaStrToDebrujinStrTest strIn strOut = do
  let out = parseFromStrToMaybe parseDebrujin strOut
  it ("check strOut " ++ strOut) $ do
    out `shouldSatisfy` isJust
  lambdaStrToDebrujinTest strIn out

lambdaToDebrujinTests = do
  anonTest
    (LAR "foo")
    Nothing
  lambdaStrToDebrujinTest
    "(/ [x] y)"
    Nothing
  lambdaStrToDebrujinTest
    "(/ [x z] y)"
    Nothing
  lambdaStrToDebrujinStrTest
    "(/ [x y] y)"
    "(/ (/ 1))"
  lambdaStrToDebrujinStrTest
    "(/ [y x] y)"
    "(/ (/ 2))"
  lambdaStrToDebrujinTest
    "(x y)"
    Nothing
  lambdaStrToDebrujinStrTest 
    "(/ [x] x)"
    "(/ 1)"
  lambdaStrToDebrujinTest 
    "(/ [x] (/ [x] x))"
    Nothing
  lambdaStrToDebrujinTest 
    "(/ [x x] x)"
    Nothing
  lambdaStrToDebrujinTest 
    "(/ [a b c] (/ [c d e] e))"
    Nothing
  lambdaStrToDebrujinStrTest 
    "(/ [y] (/ [x] (x y)))"
    "(/ (/ (1 2)))"
  lambdaStrToDebrujinStrTest 
    "(/ [y] (/ [x] (x y x y)))"
    "(/ (/ (1 2 1 2)))"
  lambdaStrToDebrujinStrTest 
    "(/ [y] (/ [x] x))"
    "(/ (/ 1))"
  lambdaStrToDebrujinStrTest 
    "((/ [x] x) (/ [x] x))"
    "((/ 1) (/ 1))"
  lambdaStrToDebrujinStrTest 
    "((/ [x] (/ [y] x)) (/ [x] x))"
    "((/ (/ 2)) (/ 1))"
  lambdaStrToDebrujinStrTest 
    "(/ [m] (/ [n] (/ [f] (/ [x] ((m f) ((n f) x))))))"
    "(/ (/ (/ (/ ((4 2) ((3 2) 1))))))"
  lambdaStrToDebrujinStrTest 
    "(/ [m n f x] (m f (n f x)))"
    "(/ (/ (/ (/ (4 2 (3 2 1))))))"
  lambdaStrToDebrujinStrTest 
    "(/ [f] (/ [x] x))"
    "(/ (/ 1))"
  lambdaStrToDebrujinStrTest 
    "(/ [f] (/ [x] (f x)))"
    "(/ (/ (2 1)))"
  lambdaStrToDebrujinStrTest 
    "(/ [f] (/ [x] (f (f x))))"
    "(/ (/ (2 (2 1))))"
  lambdaStrToDebrujinStrTest 
    "(/ [f] (/ [x] (f (f (f x)))))"
    "(/ (/ (2 (2 (2 1)))))"