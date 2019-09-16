
module DebrujinTests 
(
  debrujinTests
)
where

import Test.Hspec

import Debrujin

debrujinTests = do
  it "debrujinTests" $ do
    unParseDebrujin (DAR 1) `shouldBe` "1"
    unParseDebrujin (DAB (DAR 1)) `shouldBe` "(/ 1)"
    unParseDebrujin (DAP (DAR 1) (DAR 1)) `shouldBe` "(1 1)"
    unParseDebrujin (DAP (DAB (DAR 1)) (DAR 1)) `shouldBe` "((/ 1) 1)"
    unParseDebrujin (DAP (DAR 1) (DAP (DAB (DAR 1)) (DAR 1))) `shouldBe` "(1 ((/ 1) 1))"