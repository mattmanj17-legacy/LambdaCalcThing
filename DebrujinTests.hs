
module DebrujinTests 
(
  debrujinTests
)
where

import Test.Hspec

import Debrujin
import ParseLambdaLike
import ParseCommon
import TestHelpers

unParseDebrujinStringTest :: String -> String -> SpecWith ()
unParseDebrujinStringTest inStr outStr =
  runUnaryTestWithMaybeInput 
    (\debrujin -> it ("unParseDebrujinStringTest " ++ inStr ++ " " ++ outStr) $ do unParseDebrujin debrujin `shouldBe` outStr) 
    (parseFromStrToMaybe parseDebrujin inStr)

unParseDebrujinStringIdempotentTest :: String -> SpecWith ()
unParseDebrujinStringIdempotentTest str = 
  unParseDebrujinStringTest str str

debrujinTests = do
  it "(DAR 1) == 1" $ do
    unParseDebrujin (DAR 1) `shouldBe` "1"
  unParseDebrujinStringIdempotentTest "(/ 1)"
  unParseDebrujinStringTest "(/1  )" "(/ 1)"
  unParseDebrujinStringIdempotentTest "(1 1)"
  unParseDebrujinStringTest "(1   1 )" "(1 1)"
  unParseDebrujinStringIdempotentTest "((/ 1) 1)"
  unParseDebrujinStringIdempotentTest "(1 ((/ 1) 1))"