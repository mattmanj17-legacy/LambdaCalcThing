{-# OPTIONS_GHC -Wall #-}

import LambdaAst
import Fallible
import ParseLambda
--import UnAnonLambda
import ReduceLambda
import ParseCommon
import Data.Functor.Identity

testResults :: FallibleT IO [String]
testResults = do
  tests' <- tests
  return $ map honkhonk tests'

honkhonk :: FallibleT Identity () -> String
honkhonk x =
  case runIdentity $ runFallible x of
    (Left err) -> err
    (Right ()) -> "test passed"

tests :: FallibleT IO [FallibleT Identity ()]
tests = do
  str <- fallibleLiftM $ readFile "test.txt"
  parsed <- parseFallible parseLambda "test.txt" str
  compiled <- fallibleLiftId $ anonLambda parsed
  reduced <- fallibleLiftId $ lambdaBetaReducedFull compiled
  case reduced of
    (ExprList elems) -> return $ (map runTest) elems
    _ -> throwE "test.txt did not evaluate to a list!!!"

runTest :: Expr -> FallibleT Identity ()
runTest (ExprList [a, b])
  | a == b = return ()
  | otherwise = throwE "were not equal!!!"
runTest _ = throwE "expr was not a pair!!!"