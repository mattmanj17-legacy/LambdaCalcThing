{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module ReduceLambda where

import LambdaAst
import Control.Monad.Trans.Except
import Data.Maybe

import Text.Parsec.Pos

import Control.Monad.Writer

errorStrAt :: Ast -> String
errorStrAt ast = 
  concat
    [ "<"
    , (show (sourceLine start))
    , ":"
    , (show (sourceColumn start))
    , "-"
    , (show (sourceLine end))
    , ":"
    , (show (sourceColumn end))
    , ">"
    ]
  where
    start = srcposAstStart ast
    end = srcposAstEnd ast

anonLambda :: (Monad m) => Ast -> ExceptT String (WriterT [String] m) Expr
anonLambda = replaceVars []

incReps :: [(String, Int)] -> [(String, Int)]
incReps = map ((,) <$> fst <*> (+1) . snd)

replaceVars :: (Monad m) => [(String, Int)] -> Ast -> ExceptT String (WriterT [String] m) Expr
replaceVars reps expr = do
  lift $ tell ["replaceVars in " ++ show expr]
  case expr of
    (AstId {strAstId = idStr}) -> replaceVarsInId reps expr idStr
    (AstPair {astFst = frst, astSnd = scnd}) -> replaceVarsInPair reps (frst, scnd)
    (AstApplication {astFn = fn, astArg = arg}) -> replaceVarsInApp reps (fn, arg)
    AstEmptyList {} -> do
      return ExprEmptyList

replaceVarsInId :: (Monad m) => [(String, Int)] -> Ast -> String -> ExceptT String (WriterT [String] m) Expr
replaceVarsInId reps astParent str = do
  lift $ tell ["replaceVarsInId " ++ show reps ++ " " ++ show astParent ++ " " ++ show str]
  maybe (throwE $ (errorStrAt astParent) ++ " unrecognized id " ++ str) (return . ExprArgRef) (lookup str reps)

replaceVarsInPair :: (Monad m) => [(String, Int)] -> (Ast, Ast) -> ExceptT String (WriterT [String] m) Expr
replaceVarsInPair reps (astFirst, astSecond) = do
  lift $ tell ["replaceVarsInPair " ++ show reps ++ " " ++ show astFirst ++ " " ++ show astSecond]
  newFrst <- replaceVars reps astFirst
  newScnd <- replaceVars reps astSecond
  return $ ExprPair newFrst newScnd

replaceVarsInApp :: (Monad m) => [(String, Int)] -> (Ast, Ast) -> ExceptT String (WriterT [String] m) Expr
replaceVarsInApp reps (replaceIn, replaceWith) = do
  lift $ tell ["replaceVarsInApp " ++ show reps ++ " " ++ show replaceIn ++ " " ++ show replaceWith]
  case replaceIn of
    (AstApplication {astFn = fn@(AstId {strAstId = "fn"}), astArg = arg}) ->
      replaceVarsInAppFn reps fn (arg, replaceWith)
    _ ->
      replaceVarsInAppDefault reps (replaceIn, replaceWith)

replaceVarsInAppFn :: (Monad m) => [(String, Int)] -> Ast -> (Ast, Ast) -> ExceptT String (WriterT [String] m) Expr
replaceVarsInAppFn reps fn (params, body) = do
  lift $ tell ["replaceVarsInAppFn " ++ show reps ++ " " ++ show params ++ " " ++ show body]
  case params of
    (AstId {strAstId = idStr}) ->
      replaceVarsInAbsIdParam reps params idStr body
    (AstPair {astFst = frst, astSnd = scnd}) ->
      replaceVarsInAppFnParamsPair reps fn (frst, scnd, body)
    (AstEmptyList {}) ->
      throwE $ (errorStrAt params) ++ " empty params list for fn"
    _ -> 
      throwE $ (errorStrAt params) ++ " ill formed params list " ++ show params

replaceVarsInAppFnParamsPair :: (Monad m) => [(String, Int)] -> Ast -> (Ast, Ast, Ast) -> ExceptT String (WriterT [String] m) Expr
replaceVarsInAppFnParamsPair reps fn (paramsFrst, paramsScnd, body) = do
  lift $ tell ["replaceVarsInAppFnParamsPair " ++ show reps ++ " " ++ show paramsFrst ++ " " ++ show paramsScnd ++ " " ++ show body]
  case paramsFrst of
    (AstId {strAstId = idStr}) ->
      case paramsScnd of
        AstEmptyList {} ->
          replaceVarsInAbsIdParam reps paramsFrst idStr body
        _ -> 
          replaceVarsInAbsIdParam reps paramsFrst idStr (mkAstApp (mkAstApp fn paramsScnd) body)
    _ -> 
      throwE $ (errorStrAt paramsFrst) ++ " non id in params list for fn"

replaceVarsInAppDefault :: (Monad m) => [(String, Int)] -> (Ast, Ast) -> ExceptT String (WriterT [String] m) Expr
replaceVarsInAppDefault reps (replaceIn, replaceWith) = do
  lift $ tell ["replaceVarsInAppDefault " ++ show reps ++ " " ++ show replaceIn ++ " " ++ show replaceWith]
  fnReplaced <- replaceVars reps replaceIn
  argReplaced <- replaceVars reps replaceWith
  return $ ExprApplication fnReplaced argReplaced

replaceVarsInAbsIdParam :: (Monad m) => [(String, Int)] -> Ast -> String -> Ast -> ExceptT String (WriterT [String] m) Expr
replaceVarsInAbsIdParam reps astId str body = do
  lift $ tell ["replaceVarsInAbsIdParam " ++ show reps ++ " " ++ show astId ++ " " ++ show str ++ " " ++ show body]
  if isJust $ lookup str reps then
    throwE $ (errorStrAt astId) ++ " replaceVarsInAbsIdParam blew up because we were going to shadow a param"
  else do
    let newreps = (str, 1):(incReps reps)
    newBody <- replaceVars newreps body
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