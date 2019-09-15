
module DebrujinTests 
(
  debrujinTests
)
where

import Test.Hspec

import Debrujin

debrujinTests = do
  it "debrujinTests" $ do
    show (DAR 1) `shouldBe` "1"
    show (DAB (DAR 1)) `shouldBe` "(/ 1)"
    show (DAP (DAR 1) (DAR 1)) `shouldBe` "(1 1)"
    show (DAP (DAB (DAR 1)) (DAR 1)) `shouldBe` "((/ 1) 1)"
    show (DAP (DAR 1) (DAP (DAB (DAR 1)) (DAR 1))) `shouldBe` "(1 ((/ 1) 1))"