
module DebrujinToLambdaTests 
(
  debrujinToLambdaTests
)
where

import Test.Hspec
import Data.Maybe

import Debrujin
import Lambda
import DebrujinToLambda
import TestHelpers
import ParseCommon
import ParseLambdaLike

debrujinToLambdaTest :: Debrujin -> Maybe Lambda -> SpecWith ()
debrujinToLambdaTest debrujin lambda = 
  it ("debrujinToLambdaTest " ++ show debrujin ++ " " ++ show lambda) $ do
    (debrujinToLambda debrujin) `shouldBe` lambda

maybeDebrujinToLambdaTest :: Maybe Debrujin -> Maybe Lambda -> SpecWith ()
maybeDebrujinToLambdaTest = runBinaryTestWithMaybeInput debrujinToLambdaTest

debrujinStrToLambdaTest :: String -> Maybe Lambda -> SpecWith ()
debrujinStrToLambdaTest strIn out = do
  let parsedIn = parseFromStrToMaybe parseDebrujin strIn
  maybeDebrujinToLambdaTest parsedIn out

debrujinStrToLambdaStrTest :: String -> String -> SpecWith ()
debrujinStrToLambdaStrTest strIn strOut = do
  let out = parseFromStrToMaybe parseLambda strOut
  it ("check strOut " ++ strOut) $ do
    out `shouldSatisfy` isJust
  debrujinStrToLambdaTest strIn out

debrujinToLambdaTests = do
  debrujinToLambdaTest
    (DAR 1)
    Nothing
  debrujinStrToLambdaStrTest 
    "(/ 1)"
    "(/ [a] a)"
  debrujinStrToLambdaStrTest 
    "(/ (/ (1 2)))"
    "(/ [a] (/ [b] (b a)))"
  debrujinStrToLambdaStrTest 
    "(/ (/ 1))"
    "(/ [a] (/ [b] b))"
  debrujinStrToLambdaStrTest 
    "((/ 1) (/1))"
    "((/ [a] a) (/ [a] a))"
  debrujinStrToLambdaStrTest 
    "((/ (/ 2)) (/ 1))"
    "((/ [a] (/ [b] a)) (/ [a] a))"
  debrujinStrToLambdaStrTest 
    "(/ (/ (/ (/ ((4 2) ((3 2) 1))))))"
    "(/ [a] (/ [b] (/ [c] (/ [d] ((a c) ((b c) d))))))"
  debrujinStrToLambdaStrTest 
    "(/ (/ 1))"
    "(/ [a] (/ [b] b))"
  debrujinStrToLambdaStrTest 
    "(/ (/ (2 1)))"
    "(/ [a] (/ [b] (a b)))"
  debrujinStrToLambdaStrTest 
    "(/ (/ (2 (2 1))))"
    "(/ [a] (/ [b] (a (a b))))"
  debrujinStrToLambdaStrTest 
    "(/ (/ (2 (2 (2 1)))))"
    "(/ [a] (/ [b] (a (a (a b)))))"
    