
module DebrujinToLambdaTests 
(
  debrujinToLambdaTests
)
where

import Test.Hspec

import Debrujin
import Lambda
import DebrujinToLambda

debrujinToLambdaTest :: Debrujin -> Maybe Lambda -> SpecWith ()
debrujinToLambdaTest debrujin lambda = 
  it ("debrujinToLambdaTest " ++ show debrujin ++ " " ++ show lambda) $ do
    (debrujinToLambda debrujin) `shouldBe` lambda

debrujinToLambdaTests = do
  debrujinToLambdaTest
    (DAR 1)
    Nothing
  debrujinToLambdaTest 
    (DAB (DAR 1))
    (Just (LAB "a" (LAR "a")))
  debrujinToLambdaTest 
    (DAB (DAB (DAP (DAR 1) (DAR 2))))
    (Just (LAB "a" (LAB "b" (LAP (LAR "b") (LAR "a")))))
  debrujinToLambdaTest 
    (DAB (DAB (DAR 1)))
    (Just (LAB "a" (LAB "b" (LAR "b"))))
  debrujinToLambdaTest 
    (DAP (DAB (DAR 1)) (DAB (DAR 1)))
    (Just (LAP (LAB "a" (LAR "a")) (LAB "a" (LAR "a"))) )
  debrujinToLambdaTest 
    (DAP (DAB (DAB (DAR 2))) (DAB (DAR 1)))
    (Just (LAP (LAB "a" (LAB "b" (LAR "a"))) (LAB "a" (LAR "a"))))
  debrujinToLambdaTest 
    (DAB (DAB (DAB (DAB (DAP (DAP (DAR 4) (DAR 2)) (DAP (DAP (DAR 3) (DAR 2)) (DAR 1)))))))
    (Just (LAB "a" (LAB "b" (LAB "c" (LAB "d" (LAP (LAP (LAR "a") (LAR "c")) (LAP (LAP (LAR "b") (LAR "c")) (LAR "d"))))))))
  debrujinToLambdaTest 
    (DAB (DAB (DAR 1)))
    (Just (LAB "a" (LAB "b" (LAR "b"))))
  debrujinToLambdaTest 
    (DAB (DAB (DAP (DAR 2) (DAR 1))))
    (Just (LAB "a" (LAB "b" (LAP (LAR "a") (LAR "b")))))
  debrujinToLambdaTest 
    (DAB (DAB (DAP (DAR 2) (DAP (DAR 2) (DAR 1)))))
    (Just (LAB "a" (LAB "b" (LAP (LAR "a") (LAP (LAR "a") (LAR "b")))))) 
  debrujinToLambdaTest 
    (DAB (DAB (DAP (DAR 2) (DAP (DAR 2) (DAP (DAR 2) (DAR 1))))))
    (Just (LAB "a" (LAB "b" (LAP (LAR "a") (LAP (LAR "a") (LAP (LAR "a") (LAR "b")))))))
    