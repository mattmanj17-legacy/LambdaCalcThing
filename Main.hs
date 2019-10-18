{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

import LambdaExpr
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
    (Right failures) -> do
      _ <- sequence $ (map putStrLn) failures
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
flattenList expr =
  case expr of
    ExprPair {} ->
      frst':flattenedSnd
      where
        frst' = getFstExpr expr
        scnd' = getSndExpr expr
        flattenedSnd = flattenList scnd'
    ExprEmptyList {} ->
      []
    _ ->
      [expr]

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
    epair@(ExprPair {}) -> return $ (map runTest) (zip [0..] (flattenList epair))
    _ -> throwE "test.txt did not evaluate to a list!!!"

runTest :: (Int, Expr) -> ExceptT String Maybe (Int, Expr, Expr)
runTest (iTest, expr) =
  if exprIsPair && sndIsPair && sndSndExprIsEmptyList then
    if fstExpr == sndFstExpr then
      lift Nothing
    else
      return (iTest, fstExpr, sndFstExpr)
  else
    throwE "expr was not a pair!!!"
  where
    exprIsPair = isExprPair expr
    fstExpr = getFstExpr expr
    sndExpr = getSndExpr expr
    sndIsPair = isExprPair sndExpr
    sndFstExpr = getFstExpr sndExpr
    sndSndExpr = getSndExpr sndExpr
    sndSndExprIsEmptyList = isExprEmptyList sndSndExpr