
module LambdaTests 
(
  lambdaTests
)
where
  
import Test.Hspec

import Lambda
import TestHelpers
import ParseLambdaLike
import ParseCommon

unParseLambdaStringTest :: String -> String -> SpecWith ()
unParseLambdaStringTest inStr outStr =
  runUnaryTestWithMaybeInput 
    (\lambda -> it ("unParseLambdaStringTest " ++ inStr ++ " " ++ outStr) $ do unParseLambda lambda `shouldBe` outStr) 
    (parseFromStrToMaybe parseLambda inStr)

unParseLambdaStringIdempotentTest :: String -> SpecWith ()
unParseLambdaStringIdempotentTest str = 
  unParseLambdaStringTest str str

lambdaTests = do
  it "lambdaTests" $ do
    unParseLambda (LAR "foo") `shouldBe` "foo"
  unParseLambdaStringIdempotentTest "(/ [foo] bar)"
  unParseLambdaStringIdempotentTest "(/ [foo bonk] bar)"
  unParseLambdaStringIdempotentTest "(foo bar)"
  unParseLambdaStringIdempotentTest "(foo bar bonk)"
  unParseLambdaStringIdempotentTest "((/ [a] b) c)"
  unParseLambdaStringIdempotentTest "(a ((/ [b] c) d))"