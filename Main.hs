{-# OPTIONS_GHC -Wall #-}

import LambdaAst
import Fallible
import ParseLambda
--import UnAnonLambda
import ReduceLambda
import ParseCommon
import Data.Maybe

import Control.Monad.Writer

doIf :: Monad m => Bool -> m () -> m ()
doIf cond action = if cond then do action else return ()

main :: IO ()
main = do
  let showLog = False
  (testReults', logged) <- runWriterT $ runFallible $ testResults
  doIf showLog $ do
    putStrLn "log"
    _ <- sequence $ (map putStrLn) logged
    return ()
  case testReults' of
    (Left err) -> putStrLn err
    (Right []) -> putStrLn "all tests passed"
    (Right failurs) -> do
      _ <- sequence $ (map putStrLn) failurs
      return ()

testResults :: FallibleT (WriterT [String] IO) [String]
testResults = do
  tests' <- tests
  return $ catMaybes (map honkhonk tests')

honkhonk :: FallibleT Maybe (Int, Expr, Expr) -> Maybe String
honkhonk x =
  case runFallible x of
    Nothing -> Nothing
    (Just y) ->
      case y of
        (Left err) -> Just err
        (Right (iTest, a, b)) -> 
          Just $ 
            "test " ++ 
            show iTest ++ 
            " failed! \n\texpected\n\t\t" ++
            show b ++ 
            "\n\tbut got\n\t\t" ++
            show a

flattenList :: Expr -> [Expr]
flattenList (ExprPair frst scnd) = frst:(flattenList scnd)
flattenList ExprEmptyList = []
flattenList expr = [expr]

bongo :: FallibleT (Writer [String]) a -> FallibleT (WriterT [String] IO) a
bongo x = FallibleT $ WriterT $ return $ runWriter $ runFallible x

tests :: FallibleT (WriterT [String] IO) [FallibleT Maybe (Int, Expr, Expr)]
tests = do
  str <- fallibleLiftM $ writerLiftM $ readFile "test.txt"
  parsed <- parseFallible parseLambda "test.txt" str
  compiled <- bongo $ anonLambda parsed
  reduced <- fallibleLiftId $ lambdaBetaReducedFull compiled
  case reduced of
    epair@(ExprPair _ _) -> return $ (map runTest) (zip [0..] (flattenList epair))
    _ -> throwE "test.txt did not evaluate to a list!!!"

runTest :: (Int, Expr) -> FallibleT Maybe (Int, Expr, Expr)
runTest (iTest, (ExprPair a (ExprPair b ExprEmptyList)))
  | a == b = fallibleLiftM Nothing
  | otherwise = return (iTest, a, b)
runTest _ = throwE "expr was not a pair!!!"