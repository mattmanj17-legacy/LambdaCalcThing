
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

anonTest :: Lambda -> Maybe Debrujin -> SpecWith ()
anonTest lambda debrujin =
   it ("anonTest " ++ show lambda ++ " " ++ show debrujin) $ do
    (lambdaBoundVarsAnonymized lambda) `shouldBe` debrujin

lambdaToDebrujinTests = do
  anonTest
    (LAR "foo")
    Nothing
  anonTest
    (LAB ["x"] (LAR "y"))
    Nothing
  anonTest
    (LAB ["x", "z"] (LAR "y"))
    Nothing
  anonTest
    (LAB ["x", "y"] (LAR "y"))
    (Just (DAB (DAB (DAR 1))))
  anonTest
    (LAB ["y", "x"] (LAR "y"))
    (Just (DAB (DAB (DAR 2))))
  anonTest
    (LAP [(LAR "x"), (LAR "y")])
    Nothing
  anonTest 
    (LAB ["x"] (LAR "x"))
    (Just (DAB (DAR 1)))
  anonTest 
    (LAB ["x"] (LAB ["x"] (LAR "x")))
    Nothing
  anonTest 
    (LAB ["x", "x"] (LAR "x"))
    Nothing
  anonTest 
    (LAB ["a", "b", "c"] (LAB ["c", "d", "e"] (LAR "e")))
    Nothing
  anonTest 
    (LAB ["y"] (LAB ["x"] (LAP [(LAR "x"), (LAR "y")])))
    (Just (DAB (DAB (DAP (DAR 1) (DAR 2)))))
  anonTest 
    (LAB ["y"] (LAB ["x"] (LAP [(LAR "x"), (LAR "y"), (LAR "x"), (LAR "y")])))
    (Just (DAB (DAB (DAP (DAP (DAP (DAR 1) (DAR 2)) (DAR 1)) (DAR 2)))))
  anonTest 
    (LAB ["y"] (LAB ["x"] (LAR "x")))
    (Just (DAB (DAB (DAR 1))))
  anonTest 
    (LAP [(LAB ["x"] (LAR "x")), (LAB ["x"] (LAR "x"))]) 
    (Just (DAP (DAB (DAR 1)) (DAB (DAR 1))))
  anonTest 
    (LAP [(LAB ["x"] (LAB ["y"] (LAR "x"))), (LAB ["x"] (LAR "x"))]) 
    (Just (DAP (DAB (DAB (DAR 2))) (DAB (DAR 1))))
  anonTest 
    (LAB ["m"] (LAB ["n"] (LAB ["f"] (LAB ["x"] (LAP [(LAP [(LAR "m"), (LAR "f")]), (LAP [(LAP [(LAR "n"), (LAR "f")]), (LAR "x")])])))))
    (Just (DAB (DAB (DAB (DAB (DAP (DAP (DAR 4) (DAR 2)) (DAP (DAP (DAR 3) (DAR 2)) (DAR 1))))))))
  anonTest 
    (LAB ["m", "n", "f", "x"] (LAP [(LAR "m"), (LAR "f"), (LAP [(LAR "n"), (LAR "f"), (LAR "x")])]))
    (Just (DAB (DAB (DAB (DAB (DAP (DAP (DAR 4) (DAR 2)) (DAP (DAP (DAR 3) (DAR 2)) (DAR 1))))))))
  anonTest 
    (LAB ["f"] (LAB ["x"] (LAR "x"))) 
    (Just (DAB (DAB (DAR 1))))
  anonTest 
    (LAB ["f"] (LAB ["x"] (LAP [(LAR "f"), (LAR "x")])))
    (Just (DAB (DAB (DAP (DAR 2) (DAR 1)))))
  anonTest 
    (LAB ["f"] (LAB ["x"] (LAP [(LAR "f"), (LAP [(LAR "f"), (LAR "x")])]))) 
    (Just (DAB (DAB (DAP (DAR 2) (DAP (DAR 2) (DAR 1))))))
  anonTest 
    (LAB ["f"] (LAB ["x"] (LAP [(LAR "f"), (LAP [(LAR "f"), (LAP [(LAR "f"), (LAR "x")])])])))
    (Just (DAB (DAB (DAP (DAR 2) (DAP (DAR 2) (DAP (DAR 2) (DAR 1)))))))