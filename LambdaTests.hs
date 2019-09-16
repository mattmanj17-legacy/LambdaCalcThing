
module LambdaTests 
(
  lambdaTests
)
where
  
import Test.Hspec

import Lambda

lambdaTests = do
  it "lambdaTests" $ do
    unParseLambda (LAR "foo") `shouldBe` "foo"
    unParseLambda (LAB "foo" (LAR "bar")) `shouldBe` "(/ foo bar)"
    unParseLambda (LAP (LAR "foo") (LAR "bar")) `shouldBe` "(foo bar)"
    unParseLambda (LAP (LAB "a" (LAR "b")) (LAR "c")) `shouldBe` "((/ a b) c)"
    unParseLambda (LAP (LAR "a") (LAP (LAB "b" (LAR "c")) (LAR "d"))) `shouldBe` "(a ((/ b c) d))"