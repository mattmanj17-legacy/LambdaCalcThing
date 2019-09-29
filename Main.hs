{-# OPTIONS_GHC -Wall #-}

import LambdaAst
import Fallible
import ParseLambda
--import UnAnonLambda
import ReduceLambda
import ParseCommon
import Data.Maybe

main :: IO ()
main = do
  testReults' <- runFallible testResults
  case testReults' of
    (Left err) -> putStrLn err
    (Right []) -> putStrLn "all tests passed"
    (Right failurs) -> do
      _ <- sequence $ (map putStrLn) failurs
      return ()

testResults :: FallibleT IO [String]
testResults = do
  tests' <- tests
  return $ catMaybes (map honkhonk tests')

honkhonk :: FallibleT Maybe Int -> Maybe String
honkhonk x =
  case runFallible x of
    Nothing -> Nothing
    (Just y) ->
      case y of
        (Left err) -> Just err
        (Right iTest) -> Just $ "test " ++ show iTest ++ " failed"

tests :: FallibleT IO [FallibleT Maybe Int]
tests = do
  str <- fallibleLiftM $ readFile "test.txt"
  parsed <- parseFallible parseLambda "test.txt" str
  compiled <- fallibleLiftId $ anonLambda parsed
  reduced <- fallibleLiftId $ lambdaBetaReducedFull compiled
  case reduced of
    (ExprList elems) -> return $ (map runTest) (zip [0..] elems)
    _ -> throwE "test.txt did not evaluate to a list!!!"

runTest :: (Int, Expr) -> FallibleT Maybe Int
runTest (iTest, (ExprList [a, b]))
  | a == b = fallibleLiftM Nothing
  | otherwise = return iTest
runTest _ = throwE "expr was not a pair!!!"