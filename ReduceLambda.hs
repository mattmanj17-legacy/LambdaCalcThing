{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module ReduceLambda where

import LambdaAst
import Control.Monad.Trans.Except
import Data.Maybe

import Text.Parsec.Pos

import Control.Monad.Writer

errorStrAt :: SourcePos -> SourcePos -> String
errorStrAt start end = 
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

anonLambda :: (Monad m) => Ast -> ExceptT String (WriterT [String] m) Expr
anonLambda = replaceVars []

incReps :: [(String, Int)] -> [(String, Int)]
incReps = map ((,) <$> fst <*> (+1) . snd)

replaceVars :: (Monad m) => [(String, Int)] -> Ast -> ExceptT String (WriterT [String] m) Expr
replaceVars reps expr = do
  lift $ tell ["replaceVars in " ++ show expr] -- example of lift, keeping around
  case expr of
    (AstId sp ep str) -> replaceVarsInId reps sp ep str
    (AstPair {frst, scnd}) -> replaceVarsInPair reps (frst, scnd)
    (AstApplication {fn, arg}) -> replaceVarsInApp reps (fn, arg)
    AstEmptyList {} -> do
      return ExprEmptyList

replaceVarsInId :: (Monad m) => [(String, Int)] -> SourcePos -> SourcePos -> String -> ExceptT String (WriterT [String] m) Expr
replaceVarsInId reps sp ep str = do
  maybe (throwE $ (errorStrAt sp ep) ++ " unrecognized id " ++ str) (return . ExprArgRef) (lookup str reps)

replaceVarsInPair :: (Monad m) => [(String, Int)] -> (Ast, Ast) -> ExceptT String (WriterT [String] m) Expr
replaceVarsInPair reps (frst, scnd) = do
  newFrst <- replaceVars reps frst
  newScnd <- replaceVars reps scnd
  return $ ExprPair newFrst newScnd

replaceVarsInApp :: (Monad m) => [(String, Int)] -> (Ast, Ast) -> ExceptT String (WriterT [String] m) Expr
replaceVarsInApp reps (replaceIn, replaceWith) = do
  case replaceIn of
    (AstApplication {fn = fn'@(AstId {idStr = "fn"}), arg}) ->
      case arg of
        (AstId { idStr }) ->
          replaceVarsInAbsIdParam reps (startPos arg) (endPos arg) idStr replaceWith
        (AstPair {frst = frst'@(AstId {idStr = arg0}), scnd = AstEmptyList {}}) ->
          replaceVarsInAbsIdParam reps (startPos frst') (endPos frst') arg0 replaceWith
        (AstPair {frst = frst'@(AstId {idStr = arg0}), scnd}) ->
          let
            body = 
              (AstApplication
                (startPos fn') (endPos replaceWith)
                (AstApplication
                  (startPos fn') (endPos scnd)
                  fn'
                  scnd
                )
                replaceWith
              )
          in
            replaceVarsInAbsIdParam reps (startPos frst') (endPos frst') arg0 body
        (AstPair { frst }) ->
          throwE $ (errorStrAt (startPos frst) (endPos frst)) ++ " non id in params list for fn"
        (AstEmptyList {}) ->
          throwE $ (errorStrAt (startPos arg) (endPos arg)) ++ " empty params list for fn"
        _ -> 
          throwE $ (errorStrAt (startPos arg) (endPos arg)) ++ " ill formed params list " ++ show arg
    _ ->
      replaceVarsInAppDefault reps (replaceIn, replaceWith)

replaceVarsInAppDefault :: (Monad m) => [(String, Int)] -> (Ast, Ast) -> ExceptT String (WriterT [String] m) Expr
replaceVarsInAppDefault reps (replaceIn, replaceWith) = do
  fnReplaced <- replaceVars reps replaceIn
  argReplaced <- replaceVars reps replaceWith
  return $ ExprApplication fnReplaced argReplaced

replaceVarsInAbsIdParam :: (Monad m) => [(String, Int)] -> SourcePos -> SourcePos -> String -> Ast -> ExceptT String (WriterT [String] m) Expr
replaceVarsInAbsIdParam reps sp ep str body = do
  if isJust $ lookup str reps then
    throwE $ (errorStrAt sp ep) ++ " replaceVarsInAbsIdParam blew up because we were going to shadow a param"
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

lambdaBetaReducedOneStep (ExprPair frst scnd) = do
  newFrst <- lambdaBetaReducedOneStep frst
  newScnd <- lambdaBetaReducedOneStep scnd
  if newFrst == frst then
    return (ExprPair frst newScnd)
  else
    return (ExprPair newFrst scnd)

lambdaBetaReducedOneStep (ExprAbstraction val) = do
  reducedValOnce <- lambdaBetaReducedOneStep val
  return (ExprAbstraction reducedValOnce)

lambdaBetaReducedOneStep (ExprApplication (ExprAbstraction func) arg) =
  lambdaAppliedTo arg func

lambdaBetaReducedOneStep (ExprApplication func arg) = do
  reduced <- lambdasBetaReducedOneStep [func, arg]
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
lambdaArgRefReplacedWithLambda argRefReplace arg (ExprArgRef argRef) = do
  if argRefReplace == argRef then do
    inced <- lambdaIncrementedArgRefsGreaterThanOrEqual arg 1 argRef
    return inced
  else if argRefReplace < argRef then
    return (ExprArgRef (argRef-1))
  else
    return (ExprArgRef argRef)

lambdaArgRefReplacedWithLambda argRefReplace arg (ExprPair frst scnd) = do
  frstReplaced <- lambdaArgRefReplacedWithLambda argRefReplace arg frst
  scndReplaced <- lambdaArgRefReplacedWithLambda argRefReplace arg scnd
  return (ExprPair frstReplaced scndReplaced)

lambdaArgRefReplacedWithLambda argRefReplace arg (ExprAbstraction body) = do
  newBody <- lambdaArgRefReplacedWithLambda (argRefReplace+1) arg body
  return (ExprAbstraction newBody)

lambdaArgRefReplacedWithLambda argRefReplace argReplace (ExprApplication func arg) = do
  funcReplaced <- lambdaArgRefReplacedWithLambda argRefReplace argReplace func
  argReplaced <- lambdaArgRefReplacedWithLambda argRefReplace argReplace arg
  return (ExprApplication funcReplaced argReplaced)

lambdaArgRefReplacedWithLambda _ _ ExprEmptyList = do
  return ExprEmptyList
  

lambdaIncrementedArgRefsGreaterThanOrEqual :: (Monad m) => Expr -> Int -> Int -> ExceptT String m Expr
lambdaIncrementedArgRefsGreaterThanOrEqual lar@(ExprArgRef argRef) argRefPatchMin argRefReplacing
  | argRef < argRefPatchMin = return lar
  | otherwise = return (ExprArgRef (argRef + argRefReplacing - 1))

lambdaIncrementedArgRefsGreaterThanOrEqual (ExprPair frst scnd) argRefPatchMin argRefReplacing = do
  let incElem elem' = lambdaIncrementedArgRefsGreaterThanOrEqual elem' argRefPatchMin argRefReplacing
  frstInced <- incElem frst
  scndInced <- incElem scnd
  return (ExprPair frstInced scndInced)

lambdaIncrementedArgRefsGreaterThanOrEqual (ExprAbstraction body) argRefPatchMin argRefReplacing = do
  incedBody <- lambdaIncrementedArgRefsGreaterThanOrEqual body (argRefPatchMin + 1) argRefReplacing
  return (ExprAbstraction incedBody)

lambdaIncrementedArgRefsGreaterThanOrEqual (ExprApplication func arg) argRefPatchMin argRefReplacing = do
  let incTerm term = lambdaIncrementedArgRefsGreaterThanOrEqual term argRefPatchMin argRefReplacing
  incedFunc <- incTerm func
  incedArg <- incTerm arg
  return (ExprApplication incedFunc incedArg)

lambdaIncrementedArgRefsGreaterThanOrEqual ExprEmptyList _ _ = do
  return ExprEmptyList