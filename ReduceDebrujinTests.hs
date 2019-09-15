
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

addOperator :: Debrujin
addOperator = (DAB (DAB (DAB (DAB (DAP (DAP (DAR 4) (DAR 2)) (DAP (DAP (DAR 3) (DAR 2)) (DAR 1)))))))

addExprTest :: Int -> Int -> SpecWith ()
addExprTest = binIntOpTest "add" addOperator (+)

multOperator :: Debrujin
multOperator = (DAB (DAB (DAB (DAP (DAR 3) (DAP (DAR 2) (DAR 1))))))

multExprTest :: Int -> Int -> SpecWith ()
multExprTest = binIntOpTest "mul" multOperator (*)

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
  addExprTest 1 0
  addExprTest 1 1
  addExprTest 1 2
  addExprTest 2 1
  addExprTest 3 2
  addExprTest 2 3

  multExprTest 0 0
  multExprTest 0 1
  multExprTest 1 0
  
  multExprTest 1 1
  multExprTest 1 2
  multExprTest 2 1
  multExprTest 3 2
  multExprTest 2 3