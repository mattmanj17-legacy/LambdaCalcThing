{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

import LambdaAst
import Control.Monad.Trans.Except
import ParseLambda
import ReduceLambda
import ParseCommon
import Data.Maybe
import Data.Bool

import Control.Monad.Writer
import Control.Monad.Reader

doIf :: Monad m => Bool -> m () -> m ()
doIf cond action = if cond then do action else return ()

main :: IO ()
main = do
  let showLog = False
  (testReults', logged) <- runWriterT $ runExceptT $ testResults
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

testResults :: ExceptT String (WriterT [String] IO) [String]
testResults = do
  tests' <- tests
  return $ catMaybes (map honkhonk tests')

honkhonk :: ExceptT String Maybe (Int, Expr, Expr) -> Maybe String
honkhonk x =
  case runExceptT x of
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
flattenList (ExprPair frst' scnd') = frst':(flattenList scnd')
flattenList ExprEmptyList = []
flattenList expr = [expr]

replaceTabs :: String -> String
replaceTabs =  concatMap ((bool <$> (:[]) <*> (const "    ") <*> (=='\t')))

tests :: ExceptT String (WriterT [String] IO) [ExceptT String Maybe (Int, Expr, Expr)]
tests = do
  str <- lift $ lift $ readFile "test.txt"
  let cleanStr = replaceTabs str
  let fileLines = lines cleanStr
  parsed <- parseFallible parseLambda "test.txt" cleanStr
  let compile = runReaderT $ runWriterT $ runExceptT $ anonLambda parsed
  compiled <- ExceptT $ WriterT $ compile fileLines
  let reduced = lambdaBetaReducedFull compiled
  case reduced of
    epair@(ExprPair _ _) -> return $ (map runTest) (zip [0..] (flattenList epair))
    _ -> throwE "test.txt did not evaluate to a list!!!"

runTest :: (Int, Expr) -> ExceptT String Maybe (Int, Expr, Expr)
runTest (iTest, (ExprPair a (ExprPair b ExprEmptyList)))
  | a == b = lift Nothing
  | otherwise = return (iTest, a, b)
runTest _ = throwE "expr was not a pair!!!"