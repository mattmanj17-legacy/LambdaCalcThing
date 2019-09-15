
module ReduceDebrujinTests 
(
  reduceDebrujinTests
) 
where

import Data.Maybe
import Test.Hspec

import Debrujin
import ReduceDebrujin

reduceOnceTest :: String -> Debrujin -> Debrujin -> SpecWith ()
reduceOnceTest strId start expect =
   it ("reduceOnceTest " ++ strId) $ do
    (lambdaBetaReducedOneStep start) `shouldBe` expect

churchNum :: Int -> Debrujin
churchNum n = (DAB (DAB (churchNumHelper n)))
  where
    churchNumHelper 0 = (DAR 1)
    churchNumHelper m = (DAP (DAR 2) (churchNumHelper (m-1)))

binIntOpTest :: String -> Debrujin -> (Int -> Int -> Int) -> Int -> Int -> SpecWith ()
binIntOpTest strType expr op a b =
  it ("binIntOpTest " ++ strType ++ " " ++ show a ++ " " ++ show b) $ do 
    (lambdaBetaReducedFull (DAP (DAP expr (churchNum a)) (churchNum b))) `shouldBe` (churchNum (op a b))

addOperator = (DAB (DAB (DAB (DAB (DAP (DAP (DAR 4) (DAR 2)) (DAP (DAP (DAR 3) (DAR 2)) (DAR 1)))))))
addExprTest = binIntOpTest "add" addOperator (+)

multOperator = (DAB (DAB (DAB (DAP (DAR 3) (DAP (DAR 2) (DAR 1))))))
multExprTest = binIntOpTest "mul" multOperator (*)

powOperator = (DAB (DAB (DAB (DAB (DAP (DAP (DAP (DAR 3) (DAR 4)) (DAR 2)) (DAR 1))))))
powExprTest = binIntOpTest "pow" multOperator (^)

reduceDebrujinTests = do
  reduceOnceTest "0 fully reduced arg ref" (DAR 1) (DAR 1)
  reduceOnceTest "1 fully reduced abstraction" (DAB (DAR 1)) (DAB (DAR 1))
  reduceOnceTest "2 fully reduced application" (DAP (DAR 1) (DAR 1)) (DAP (DAR 1) (DAR 1))
  reduceOnceTest "3 (id id)" (DAP (DAB (DAR 1)) (DAB (DAR 1))) (DAB (DAR 1))
  reduceOnceTest "4 reducing body of abstraction" (DAB (DAP (DAB (DAR 1)) (DAB (DAR 1)))) (DAB (DAB (DAR 1)))
  reduceOnceTest "5 reducing func of application" (DAP (DAP (DAB (DAR 1)) (DAB (DAR 1))) (DAR 1)) (DAP (DAB (DAR 1)) (DAR 1))
  reduceOnceTest "6 reducing arg of application" (DAP (DAR 1) (DAP (DAB (DAR 1)) (DAB (DAR 1)))) (DAP (DAR 1) (DAB (DAR 1)))
  reduceOnceTest "7 ignore arg" (DAP (DAB (DAB (DAR 1))) (DAR 3)) (DAB (DAR 1))
  reduceOnceTest "8 inc arg" (DAP (DAB (DAB (DAR 2))) (DAR 3)) (DAB (DAR 4))
  reduceOnceTest "9 fours" (DAP (DAB (DAB (DAB (DAB (DAR 4))))) (DAB (DAB (DAB (DAB (DAR 4)))))) (DAB (DAB (DAB (DAB (DAB (DAB (DAB (DAR 4))))))))
  reduceOnceTest "10" (DAP (DAB (DAR 1)) (DAR 3)) (DAR 3)
  reduceOnceTest "11" (DAP (DAB (DAR 2)) (DAR 3)) (DAR 1)
  reduceOnceTest "12" (DAP (DAB (DAP (DAR 1) (DAR 1))) (DAR 3)) (DAP (DAR 3) (DAR 3))
  reduceOnceTest "13" (DAP (DAB (DAP (DAR 2) (DAR 2))) (DAR 3)) (DAP (DAR 1) (DAR 1)) 

  addExprTest 0 0
  addExprTest 0 1
  addExprTest 0 2
  addExprTest 0 3
  addExprTest 0 4
  addExprTest 1 0
  addExprTest 1 1
  addExprTest 1 2
  addExprTest 1 3
  addExprTest 1 4
  addExprTest 2 0
  addExprTest 2 1
  addExprTest 2 2
  addExprTest 2 3
  addExprTest 2 4
  addExprTest 3 0
  addExprTest 3 1
  addExprTest 3 2
  addExprTest 3 3
  addExprTest 3 4
  addExprTest 4 0
  addExprTest 4 1
  addExprTest 4 2
  addExprTest 4 3
  addExprTest 4 4

  multExprTest 0 0
  multExprTest 0 1
  multExprTest 0 2
  multExprTest 0 3
  multExprTest 0 4
  multExprTest 1 0
  multExprTest 1 1
  multExprTest 1 2
  multExprTest 1 3
  multExprTest 1 4
  multExprTest 2 0
  multExprTest 2 1
  multExprTest 2 2
  multExprTest 2 3
  multExprTest 2 4
  multExprTest 3 0
  multExprTest 3 1
  multExprTest 3 2
  multExprTest 3 3
  multExprTest 3 4
  multExprTest 4 0
  multExprTest 4 1
  multExprTest 4 2
  multExprTest 4 3
  multExprTest 4 4

  --powExprTest 0 0
  --powExprTest 0 1
  --powExprTest 0 2
  --powExprTest 0 3
  --powExprTest 0 4
  --powExprTest 1 0
  --powExprTest 1 1
  powExprTest 1 2
  --powExprTest 1 3
  --powExprTest 1 4
  --powExprTest 2 0
  --powExprTest 2 1
 -- powExprTest 2 2
  --powExprTest 2 3
  --powExprTest 2 4
  --powExprTest 3 0
  --powExprTest 3 1
  --powExprTest 3 2
  --powExprTest 3 3
  --powExprTest 3 4
  --powExprTest 4 0
  --powExprTest 4 1
  --powExprTest 4 2
  --powExprTest 4 3
  --powExprTest 4 4