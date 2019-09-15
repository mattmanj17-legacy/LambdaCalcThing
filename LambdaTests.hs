
module LambdaTests 
(
  lambdaTests
)
where
  
import Test.Hspec

import Lambda

lambdaTests = do
  it "lambdaTests" $ do
    show (LAR "foo") `shouldBe` "foo"
    show (LAB "foo" (LAR "bar")) `shouldBe` "(/ foo bar)"
    show (LAP (LAR "foo") (LAR "bar")) `shouldBe` "(foo bar)"
    show (LAP (LAB "a" (LAR "b")) (LAR "c")) `shouldBe` "((/ a b) c)"
    show (LAP (LAR "a") (LAP (LAB "b" (LAR "c")) (LAR "d"))) `shouldBe` "(a ((/ b c) d))"