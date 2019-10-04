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

lambdasBetaReducedOneStep :: (Monad m) => [Expr] -> ExceptT String m [Expr]
lambdasBetaReducedOneStep [] =
  return []

lambdasBetaReducedOneStep [term] = do
  reducedTerm <- lambdaBetaReducedOneStep term
  return [reducedTerm]

lambdasBetaReducedOneStep (term:rest) = do
  termReducedOnce <- lambdaBetaReducedOneStep term
  if term == termReducedOnce then do
    restReducedOnce <- lambdasBetaReducedOneStep rest
    return (term:restReducedOnce)
  else do
    return (termReducedOnce:rest)

lambdaBetaReducedOneStep :: (Monad m) => Expr -> ExceptT String m Expr
lambdaBetaReducedOneStep argRef@(ExprArgRef _) =
  return argRef

lambdaBetaReducedOneStep (ExprPair frst' scnd') = do
  newFrst <- lambdaBetaReducedOneStep frst'
  newScnd <- lambdaBetaReducedOneStep scnd'
  if newFrst == frst' then
    return (ExprPair newFrst newScnd)
  else
    return (ExprPair newFrst scnd')

lambdaBetaReducedOneStep (ExprAbstraction val) = do
  reducedValOnce <- lambdaBetaReducedOneStep val
  return (ExprAbstraction reducedValOnce)

lambdaBetaReducedOneStep (ExprApplication (ExprAbstraction func) arg') =
  lambdaAppliedTo arg' func

lambdaBetaReducedOneStep (ExprApplication func arg') = do
  reduced <- lambdasBetaReducedOneStep [func, arg']
  case reduced of
    [newFunc, newArg] -> return (ExprApplication newFunc newArg)
    _ -> throwE "huh??? lambdaBetaReducedOneStep blew up?"

lambdaBetaReducedOneStep ExprEmptyList = do
  return ExprEmptyList


lambdaBetaReducedFull :: (Monad m) => Expr -> ExceptT String m Expr
lambdaBetaReducedFull term = do
  reducedOnce <- lambdaBetaReducedOneStep term
  if term == reducedOnce then do
    return term
  else do
    lambdaBetaReducedFull reducedOnce

lambdaAppliedTo :: (Monad m) => Expr -> Expr -> ExceptT String m Expr
lambdaAppliedTo = 
  lambdaArgRefReplacedWithLambda 1


lambdaArgRefReplacedWithLambda :: (Monad m) => Int -> Expr -> Expr -> ExceptT String m Expr
lambdaArgRefReplacedWithLambda argRefReplace arg' (ExprArgRef argRef) = do
  if argRefReplace == argRef then do
    inced <- lambdaIncrementedArgRefsGreaterThanOrEqual arg' 1 argRef
    return inced
  else if argRefReplace < argRef then
    return (ExprArgRef (argRef-1))
  else
    return (ExprArgRef argRef)

lambdaArgRefReplacedWithLambda argRefReplace arg' (ExprPair frst' scnd') = do
  frstReplaced <- lambdaArgRefReplacedWithLambda argRefReplace arg' frst'
  scndReplaced <- lambdaArgRefReplacedWithLambda argRefReplace arg' scnd'
  return (ExprPair frstReplaced scndReplaced)

lambdaArgRefReplacedWithLambda argRefReplace arg' (ExprAbstraction body) = do
  newBody <- lambdaArgRefReplacedWithLambda (argRefReplace+1) arg' body
  return (ExprAbstraction newBody)

lambdaArgRefReplacedWithLambda argRefReplace argReplace (ExprApplication func arg') = do
  funcReplaced <- lambdaArgRefReplacedWithLambda argRefReplace argReplace func
  argReplaced <- lambdaArgRefReplacedWithLambda argRefReplace argReplace arg'
  return (ExprApplication funcReplaced argReplaced)

lambdaArgRefReplacedWithLambda _ _ ExprEmptyList = do
  return ExprEmptyList
  

lambdaIncrementedArgRefsGreaterThanOrEqual :: (Monad m) => Expr -> Int -> Int -> ExceptT String m Expr
lambdaIncrementedArgRefsGreaterThanOrEqual lar@(ExprArgRef argRef) argRefPatchMin argRefReplacing
  | argRef < argRefPatchMin = return lar
  | otherwise = return (ExprArgRef (argRef + argRefReplacing - 1))

lambdaIncrementedArgRefsGreaterThanOrEqual (ExprPair frst' scnd') argRefPatchMin argRefReplacing = do
  let incElem elem' = lambdaIncrementedArgRefsGreaterThanOrEqual elem' argRefPatchMin argRefReplacing
  frstInced <- incElem frst'
  scndInced <- incElem scnd'
  return (ExprPair frstInced scndInced)

lambdaIncrementedArgRefsGreaterThanOrEqual (ExprAbstraction body) argRefPatchMin argRefReplacing = do
  incedBody <- lambdaIncrementedArgRefsGreaterThanOrEqual body (argRefPatchMin + 1) argRefReplacing
  return (ExprAbstraction incedBody)

lambdaIncrementedArgRefsGreaterThanOrEqual (ExprApplication func arg') argRefPatchMin argRefReplacing = do
  let incTerm term = lambdaIncrementedArgRefsGreaterThanOrEqual term argRefPatchMin argRefReplacing
  incedFunc <- incTerm func
  incedArg <- incTerm arg'
  return (ExprApplication incedFunc incedArg)

lambdaIncrementedArgRefsGreaterThanOrEqual ExprEmptyList _ _ = do
  return ExprEmptyList