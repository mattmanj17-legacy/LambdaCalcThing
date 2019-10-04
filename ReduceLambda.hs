{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module ReduceLambda where

import LambdaAst
import Control.Monad.Trans.Except
import Data.Maybe

import Text.Parsec.Pos

import Control.Monad.Writer

errorStrAt :: [String] -> Ast -> String -> String
errorStrAt fileLines ast strMsg =
   unlines (((posStr ++ " " ++strMsg):viewedLines) ++ [honk])
  where
    viewedLines = (take (endLine - startLine + 1)) $ (drop (startLine - 1)) fileLines
    honk = (replicate (startChar - 1) '+') ++ (replicate (endChar - startChar) '-')
    posStr = 
      concat
        [ "<"
        , (show startLine)
        , ":"
        , (show startChar)
        , "-"
        , (show endLine)
        , ":"
        , (show endChar)
        , ">"
        ]
    startLine = (sourceLine start)
    endLine = (sourceLine end)
    startChar = (sourceColumn start)
    endChar = (sourceColumn end)
    start = srcposAstStart ast
    end = srcposAstEnd ast

anonLambda :: (Monad m) => [String] -> Ast -> ExceptT String (WriterT [String] m) Expr
anonLambda fileLines = replaceVars fileLines []

incReps :: [(String, Int)] -> [(String, Int)]
incReps = map ((,) <$> fst <*> (+1) . snd)

replaceVars :: (Monad m) => [String] -> [(String, Int)] -> Ast -> ExceptT String (WriterT [String] m) Expr
replaceVars fileLines reps expr = do
  lift $ tell ["replaceVars in " ++ show expr]
  case expr of
    (AstId {strAstId = idStr}) -> replaceVarsInId fileLines reps expr idStr
    (AstPair {astFst = frst, astSnd = scnd}) -> replaceVarsInPair fileLines reps (frst, scnd)
    (AstApplication {astFn = fn, astArg = arg}) -> replaceVarsInApp fileLines reps (fn, arg)
    AstEmptyList {} -> do
      return ExprEmptyList

replaceVarsInId :: (Monad m) => [String] -> [(String, Int)] -> Ast -> String -> ExceptT String (WriterT [String] m) Expr
replaceVarsInId fileLines reps astParent str = do
  lift $ tell ["replaceVarsInId " ++ show reps ++ " " ++ show astParent ++ " " ++ show str]
  maybe (throwE $ (errorStrAt fileLines astParent (" unrecognized id " ++ str))) (return . ExprArgRef) (lookup str reps)

replaceVarsInPair :: (Monad m) => [String] -> [(String, Int)] -> (Ast, Ast) -> ExceptT String (WriterT [String] m) Expr
replaceVarsInPair fileLines reps (astFirst, astSecond) = do
  lift $ tell ["replaceVarsInPair " ++ show reps ++ " " ++ show astFirst ++ " " ++ show astSecond]
  newFrst <- replaceVars fileLines reps astFirst
  newScnd <- replaceVars fileLines reps astSecond
  return $ ExprPair newFrst newScnd

replaceVarsInApp :: (Monad m) => [String] -> [(String, Int)] -> (Ast, Ast) -> ExceptT String (WriterT [String] m) Expr
replaceVarsInApp fileLines reps (replaceIn, replaceWith) = do
  lift $ tell ["replaceVarsInApp " ++ show reps ++ " " ++ show replaceIn ++ " " ++ show replaceWith]
  case replaceIn of
    (AstApplication {astFn = fn@(AstId {strAstId = "fn"}), astArg = arg}) ->
      replaceVarsInAppFn fileLines reps fn (arg, replaceWith)
    _ ->
      replaceVarsInAppDefault fileLines reps (replaceIn, replaceWith)

replaceVarsInAppFn :: (Monad m) => [String] -> [(String, Int)] -> Ast -> (Ast, Ast) -> ExceptT String (WriterT [String] m) Expr
replaceVarsInAppFn fileLines reps fn (params, body) = do
  lift $ tell ["replaceVarsInAppFn " ++ show reps ++ " " ++ show params ++ " " ++ show body]
  case params of
    (AstId {strAstId = idStr}) ->
      replaceVarsInAbsIdParam fileLines reps params idStr body
    (AstPair {astFst = frst, astSnd = scnd}) ->
      replaceVarsInAppFnParamsPair fileLines reps fn (frst, scnd, body)
    (AstEmptyList {}) ->
      throwE $ errorStrAt fileLines params " empty params list for fn"
    _ -> 
      throwE $ errorStrAt fileLines params (" ill formed params list " ++ show params)

replaceVarsInAppFnParamsPair :: (Monad m) => [String] -> [(String, Int)] -> Ast -> (Ast, Ast, Ast) -> ExceptT String (WriterT [String] m) Expr
replaceVarsInAppFnParamsPair fileLines reps fn (paramsFrst, paramsScnd, body) = do
  lift $ tell ["replaceVarsInAppFnParamsPair " ++ show reps ++ " " ++ show paramsFrst ++ " " ++ show paramsScnd ++ " " ++ show body]
  case paramsFrst of
    (AstId {strAstId = idStr}) ->
      case paramsScnd of
        AstEmptyList {} ->
          replaceVarsInAbsIdParam fileLines reps paramsFrst idStr body
        _ -> 
          replaceVarsInAbsIdParam fileLines reps paramsFrst idStr (mkAstApp (mkAstApp fn paramsScnd) body)
    _ -> 
      throwE $ errorStrAt fileLines paramsFrst " non id in params list for fn"

replaceVarsInAppDefault :: (Monad m) => [String] -> [(String, Int)] -> (Ast, Ast) -> ExceptT String (WriterT [String] m) Expr
replaceVarsInAppDefault fileLines reps (replaceIn, replaceWith) = do
  lift $ tell ["replaceVarsInAppDefault " ++ show reps ++ " " ++ show replaceIn ++ " " ++ show replaceWith]
  fnReplaced <- replaceVars fileLines reps replaceIn
  argReplaced <- replaceVars fileLines reps replaceWith
  return $ ExprApplication fnReplaced argReplaced

replaceVarsInAbsIdParam :: (Monad m) => [String] -> [(String, Int)] -> Ast -> String -> Ast -> ExceptT String (WriterT [String] m) Expr
replaceVarsInAbsIdParam fileLines reps astId str body = do
  lift $ tell ["replaceVarsInAbsIdParam " ++ show reps ++ " " ++ show astId ++ " " ++ show str ++ " " ++ show body]
  if isJust $ lookup str reps then
    throwE $ errorStrAt fileLines astId " replaceVarsInAbsIdParam blew up because we were going to shadow a param"
  else do
    let newreps = (str, 1):(incReps reps)
    newBody <- replaceVars fileLines newreps body
    return (ExprAbstraction newBody)

-- REDUX

lambdaBetaReducedOneStep :: Expr -> Expr
lambdaBetaReducedOneStep argRef@(ExprArgRef _) =
  argRef

lambdaBetaReducedOneStep (ExprPair frst' scnd') =
  if newFrst == frst' then
    (ExprPair newFrst newScnd)
  else
    (ExprPair newFrst scnd')
  where
    newFrst = lambdaBetaReducedOneStep frst'
    newScnd = lambdaBetaReducedOneStep scnd'

lambdaBetaReducedOneStep (ExprAbstraction val) =
  ExprAbstraction $ lambdaBetaReducedOneStep val

lambdaBetaReducedOneStep (ExprApplication (ExprAbstraction func) arg') =
  lambdaAppliedTo arg' func

lambdaBetaReducedOneStep (ExprApplication func arg') =
  if newFunc == func then
    (ExprApplication func newArg)
  else
    (ExprApplication newFunc arg')
  where
    newFunc = lambdaBetaReducedOneStep func
    newArg = lambdaBetaReducedOneStep arg'

lambdaBetaReducedOneStep ExprEmptyList =
  ExprEmptyList


lambdaBetaReducedFull :: Expr -> Expr
lambdaBetaReducedFull term = 
  if term == reducedOnce then do
    term
  else do
    lambdaBetaReducedFull reducedOnce
  where
    reducedOnce = lambdaBetaReducedOneStep term

lambdaAppliedTo :: Expr -> Expr -> Expr
lambdaAppliedTo = 
  lambdaArgRefReplacedWithLambda 1


lambdaArgRefReplacedWithLambda :: Int -> Expr -> Expr -> Expr
lambdaArgRefReplacedWithLambda argRefReplace arg' (ExprArgRef argRef) = 
  if argRefReplace == argRef then 
    lambdaIncrementedArgRefsGreaterThanOrEqual arg' 1 argRef
  else if argRefReplace < argRef then
    (ExprArgRef (argRef-1))
  else
    (ExprArgRef argRef)

lambdaArgRefReplacedWithLambda argRefReplace arg' (ExprPair frst' scnd') = 
  (ExprPair frstReplaced scndReplaced)
  where
    frstReplaced = lambdaArgRefReplacedWithLambda argRefReplace arg' frst'
    scndReplaced = lambdaArgRefReplacedWithLambda argRefReplace arg' scnd'

lambdaArgRefReplacedWithLambda argRefReplace arg' (ExprAbstraction body) = 
  ExprAbstraction $ lambdaArgRefReplacedWithLambda (argRefReplace+1) arg' body

lambdaArgRefReplacedWithLambda argRefReplace argReplace (ExprApplication func arg') = 
  (ExprApplication funcReplaced argReplaced)
  where
    funcReplaced = lambdaArgRefReplacedWithLambda argRefReplace argReplace func
    argReplaced = lambdaArgRefReplacedWithLambda argRefReplace argReplace arg'

lambdaArgRefReplacedWithLambda _ _ ExprEmptyList = 
  ExprEmptyList
  

lambdaIncrementedArgRefsGreaterThanOrEqual :: Expr -> Int -> Int -> Expr
lambdaIncrementedArgRefsGreaterThanOrEqual lar@(ExprArgRef argRef) argRefPatchMin argRefReplacing
  | argRef < argRefPatchMin = lar
  | otherwise = (ExprArgRef (argRef + argRefReplacing - 1))

lambdaIncrementedArgRefsGreaterThanOrEqual (ExprPair frst' scnd') argRefPatchMin argRefReplacing =
  (ExprPair (incElem frst') (incElem scnd'))
  where
    incElem elem' = lambdaIncrementedArgRefsGreaterThanOrEqual elem' argRefPatchMin argRefReplacing

lambdaIncrementedArgRefsGreaterThanOrEqual (ExprAbstraction body) argRefPatchMin argRefReplacing =
  ExprAbstraction $ lambdaIncrementedArgRefsGreaterThanOrEqual body (argRefPatchMin + 1) argRefReplacing

lambdaIncrementedArgRefsGreaterThanOrEqual (ExprApplication func arg') argRefPatchMin argRefReplacing =
  ExprApplication (incTerm func) (incTerm arg')
  where
    incTerm term = lambdaIncrementedArgRefsGreaterThanOrEqual term argRefPatchMin argRefReplacing

lambdaIncrementedArgRefsGreaterThanOrEqual ExprEmptyList _ _ =
  ExprEmptyList