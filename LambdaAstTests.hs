
module LambdaAstTests 
(
  lambdaAstTests
)
where
  
import Test.Hspec

import LambdaAst
import TestHelpers
import ParseLambda
import ParseCommon

unParseLambdaStringTest :: String -> String -> String -> SpecWith ()
unParseLambdaStringTest strDesc inStr outStr =
  runUnaryTestWithMaybeInput
    strDesc
    (\sd lambda -> it sd $ do show lambda `shouldBe` outStr) 
    (parseFromStrToEither parseLambda inStr)

unParseLambdaStringIdempotentTest :: String ->  String -> SpecWith ()
unParseLambdaStringIdempotentTest strDesc str = 
  unParseLambdaStringTest strDesc str str

lambdaAstTests = do
  it "unParseLambda foo" $ do
    show (LambdaId "foo") `shouldBe` "foo"
  unParseLambdaStringIdempotentTest "upl0" "(/ [foo] bar)"
  unParseLambdaStringIdempotentTest "upl1" "(/ [foo bonk] bar)"
  unParseLambdaStringIdempotentTest "upl2" "(foo bar)"
  unParseLambdaStringIdempotentTest "upl3" "(foo bar bonk)"
  unParseLambdaStringIdempotentTest "upl4" "((/ [a] b) c)"
  unParseLambdaStringIdempotentTest "upl5" "(a ((/ [b] c) d))"