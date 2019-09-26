{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module ReduceLambda where

import LambdaAst
import Data.Maybe

import Prelude hiding (fail)
import Control.Monad.Fail

-- ANON

anonLambda :: LambdaAst -> EitherStringOr LambdaAst
anonLambda = replaceVars []

incReps :: [(String, Int)] -> [(String, Int)]
incReps = map ((,) <$> fst <*> (+1) . snd)

replaceVars :: [(String, Int)] -> LambdaAst -> EitherStringOr LambdaAst
replaceVars reps repIn =
  case repIn of
    (LambdaId str) -> replaceVarsInId reps str
    (LambdaArgRef argRef) -> (return (LambdaArgRef argRef))
    (LambdaList elems) -> replaceVarsInList reps elems
    (LambdaAbstraction params body) -> replaceVarsInAbs reps params body
    (LambdaAnonAbstraction body) ->  replaceVarsInAnonAbs reps body 
    (LambdaApplication terms) -> replaceVarsInApp reps terms
    (LambdaBif fn) -> (return (LambdaBif fn))

replaceVarsInId :: [(String, Int)] -> String -> EitherStringOr LambdaAst
replaceVarsInId reps str =
  return $ maybe (LambdaId str) LambdaArgRef (lookup str reps)

replaceVarsInList :: [(String, Int)] -> [LambdaAst] -> EitherStringOr LambdaAst
replaceVarsInList reps elems = do
  newElems <- sequence $ map (replaceVars reps) elems
  return $ LambdaList newElems

replaceVarsInApp :: [(String, Int)] -> [LambdaAst] -> EitherStringOr LambdaAst
replaceVarsInApp reps terms = do
  newTerms <- sequence $ map (replaceVars reps) terms
  return $ LambdaApplication newTerms

replaceVarsInAnonAbs :: [(String, Int)] -> LambdaAst -> EitherStringOr LambdaAst
replaceVarsInAnonAbs reps body = do
  newBody <- replaceVars (incReps reps) body
  return (LambdaAnonAbstraction newBody)

replaceVarsInAbs :: [(String, Int)] -> LambdaAst -> LambdaAst -> EitherStringOr LambdaAst
replaceVarsInAbs reps params body =
  case params of
    (LambdaId str) -> replaceVarsInAbsIdParam reps str body
    (LambdaList elems) -> replaceVarsInAbsListParam reps elems body
    (LambdaApplication terms) -> replaceVarsInAbsAppParam reps terms body
    _ -> fail $ "replaceVarsInAbs blew up on " ++ (show params) ++ " params"

replaceVarsInAbsIdParam :: [(String, Int)] -> String -> LambdaAst -> EitherStringOr LambdaAst
replaceVarsInAbsIdParam reps str body = do
  if isJust $ lookup str reps then
    fail "replaceVarsInAbsIdParam blew up because we were going to shadow a param"
  else do
    let newreps = (str, 1):(incReps reps)
    newBody <- replaceVars newreps body
    return (LambdaAnonAbstraction newBody)

replaceVarsInAbsListParam :: [(String, Int)] -> [LambdaAst] -> LambdaAst -> EitherStringOr LambdaAst
replaceVarsInAbsListParam reps paramElems body =
  case paramElems of
    [] -> fail "replaceVarsInAbsListParam blew up because we got an empty params list"
    [singleElem] -> replaceVarsInAbs reps singleElem body
    (firstElem:rest) -> replaceVarsInAbs reps firstElem (LambdaAbstraction (LambdaList rest) body)

replaceVarsInAbsAppParam :: [(String, Int)] -> [LambdaAst] -> LambdaAst -> EitherStringOr LambdaAst
replaceVarsInAbsAppParam reps paramTerms body = do
  let params = (LambdaApplication paramTerms)
  paramsReducedOnce <- lambdaBetaReducedOneStep params
  if considerLambdasEqualForRedux params paramsReducedOnce then
    fail "replaceVarsInAbsAppParam blew up because we got a fully reduced LambdaApplication"
  else
    replaceVarsInAbs reps paramsReducedOnce body 

-- REDUX

lambdasBetaReducedOneStep :: [LambdaAst] -> EitherStringOr [LambdaAst]
lambdasBetaReducedOneStep [] =
  return []

lambdasBetaReducedOneStep [term] = do
  reducedTerm <- lambdaBetaReducedOneStep term
  return [reducedTerm]

lambdasBetaReducedOneStep (term:rest) = do
  termReducedOnce <- lambdaBetaReducedOneStep term
  if considerLambdasEqualForRedux term termReducedOnce then do
    restReducedOnce <- lambdasBetaReducedOneStep rest
    return (term:restReducedOnce)
  else do
    return (termReducedOnce:rest)

bifMap :: [(String, (LambdaAst -> EitherStringOr LambdaAst))]
bifMap = 
  [ ("cons", consBif)
  , ("apply", applyBif)
  , ("fn", fnBif)
  , ("letin", letinBif)
  ]

lambdaBetaReducedOneStep :: LambdaAst -> EitherStringOr LambdaAst
lambdaBetaReducedOneStep lid@(LambdaId str) = do
  let bif = lookup str bifMap
  if isJust bif then do
    return $ LambdaBif $ fromJust bif
  else do
    return lid

lambdaBetaReducedOneStep bif@(LambdaBif _) =
  return bif

lambdaBetaReducedOneStep argRef@(LambdaArgRef _) =
  return argRef

lambdaBetaReducedOneStep (LambdaList elems) = do
  elemsReducedOnce <- lambdasBetaReducedOneStep elems
  return (LambdaList elemsReducedOnce)

lambdaBetaReducedOneStep abstraction@(LambdaAbstraction params body) = do
  paramsReducedOnce <- lambdaBetaReducedOneStep params
  if considerLambdasEqualForRedux paramsReducedOnce params then do
    anonLambda abstraction
  else do
    return (LambdaAbstraction paramsReducedOnce body)

lambdaBetaReducedOneStep (LambdaAnonAbstraction val) = do
  reducedValOnce <- lambdaBetaReducedOneStep val
  return (LambdaAnonAbstraction reducedValOnce)

lambdaBetaReducedOneStep (LambdaApplication [(LambdaAnonAbstraction func), arg]) =
  lambdaAppliedTo arg func

lambdaBetaReducedOneStep (LambdaApplication [(LambdaBif fn), arg]) =
  fn arg

lambdaBetaReducedOneStep (LambdaApplication ((LambdaAnonAbstraction func):arg:rest)) = do
  applied <- lambdaAppliedTo arg func
  return (LambdaApplication (applied:rest))

lambdaBetaReducedOneStep (LambdaApplication ((LambdaBif fn):arg:rest)) = do
  applied <- fn arg
  return (LambdaApplication (applied:rest))

lambdaBetaReducedOneStep (LambdaApplication []) =
  fail "lambdaBetaReducedOneStep blew up because we had no terms in a LambdaApplication"

lambdaBetaReducedOneStep (LambdaApplication [_]) =
  fail "lambdaBetaReducedOneStep blew up because we had only one term in a LambdaApplication"

lambdaBetaReducedOneStep (LambdaApplication terms) = do
  termsReducedOnce <- lambdasBetaReducedOneStep terms
  return (LambdaApplication termsReducedOnce)


lambdaBetaReducedFull :: LambdaAst -> EitherStringOr LambdaAst
lambdaBetaReducedFull term = do
  reducedOnce <- lambdaBetaReducedOneStep term
  if considerLambdasEqualForRedux term reducedOnce then do
    return term
  else do
    lambdaBetaReducedFull reducedOnce

lambdaAppliedTo :: LambdaAst -> LambdaAst -> EitherStringOr LambdaAst
lambdaAppliedTo = 
  lambdaArgRefReplacedWithLambda 1


lambdaArgRefReplacedWithLambda :: Int -> LambdaAst -> LambdaAst -> EitherStringOr LambdaAst
lambdaArgRefReplacedWithLambda _ _ bif@(LambdaBif _) =
  return bif

lambdaArgRefReplacedWithLambda _ _ lid@(LambdaId _) =
  return lid

lambdaArgRefReplacedWithLambda argRefReplace arg (LambdaArgRef argRef) = do
  if argRefReplace == argRef then do
    inced <- lambdaIncrementedArgRefsGreaterThanOrEqual arg 1 argRef
    return inced
  else if argRefReplace < argRef then
    return (LambdaArgRef (argRef-1))
  else
    return (LambdaArgRef argRef)

lambdaArgRefReplacedWithLambda argRefReplace arg (LambdaList elems) = do
  elemsReplaced <- sequence (map (lambdaArgRefReplacedWithLambda argRefReplace arg) elems)
  return (LambdaList elemsReplaced)

lambdaArgRefReplacedWithLambda argRefReplace arg abstraction@(LambdaAbstraction _ _) = do
  anoned <- anonLambda abstraction
  lambdaArgRefReplacedWithLambda argRefReplace arg anoned

lambdaArgRefReplacedWithLambda argRefReplace arg (LambdaAnonAbstraction body) = do
  newBody <- lambdaArgRefReplacedWithLambda (argRefReplace+1) arg body
  return (LambdaAnonAbstraction newBody)

lambdaArgRefReplacedWithLambda argRefReplace argReplace (LambdaApplication terms) = do
  termsReplaced <- sequence (map (lambdaArgRefReplacedWithLambda argRefReplace argReplace) terms)
  return (LambdaApplication termsReplaced)
  

lambdaIncrementedArgRefsGreaterThanOrEqual :: LambdaAst -> Int -> Int -> EitherStringOr LambdaAst
lambdaIncrementedArgRefsGreaterThanOrEqual bif@(LambdaBif _) _ _ =
  return bif

lambdaIncrementedArgRefsGreaterThanOrEqual lid@(LambdaId _) _ _ =
  return lid

lambdaIncrementedArgRefsGreaterThanOrEqual lar@(LambdaArgRef argRef) argRefPatchMin argRefReplacing
  | argRef < argRefPatchMin = return lar
  | otherwise = return (LambdaArgRef (argRef + argRefReplacing - 1))

lambdaIncrementedArgRefsGreaterThanOrEqual (LambdaList elems) argRefPatchMin argRefReplacing = do
  let incElems lElem = lambdaIncrementedArgRefsGreaterThanOrEqual lElem argRefPatchMin argRefReplacing
  incedElems <- sequence (map incElems elems)
  return (LambdaList incedElems)

lambdaIncrementedArgRefsGreaterThanOrEqual abstraction@(LambdaAbstraction _ _) argRefPatchMin argRefReplacing = do
  anoned <- anonLambda abstraction
  lambdaIncrementedArgRefsGreaterThanOrEqual anoned argRefPatchMin argRefReplacing

lambdaIncrementedArgRefsGreaterThanOrEqual (LambdaAnonAbstraction body) argRefPatchMin argRefReplacing = do
  incedBody <- lambdaIncrementedArgRefsGreaterThanOrEqual body (argRefPatchMin + 1) argRefReplacing
  return (LambdaAnonAbstraction incedBody)

lambdaIncrementedArgRefsGreaterThanOrEqual (LambdaApplication terms) argRefPatchMin argRefReplacing = do
  let incTerm term = lambdaIncrementedArgRefsGreaterThanOrEqual term argRefPatchMin argRefReplacing
  incedTerms <- sequence (map incTerm terms)
  return (LambdaApplication incedTerms)

-- bifs

considerLambdasEqualForRedux :: LambdaAst -> LambdaAst -> Bool
considerLambdasEqualForRedux = curry $ (boolFromTfn True) . (tfnCompareLambdas <$> fst <*> snd)

tryReduceToList :: LambdaAst -> EitherStringOr LambdaAst
tryReduceToList list@(LambdaList _) = do
  return list

tryReduceToList expr = do
  exprReducedOnce <- lambdaBetaReducedOneStep expr
  if not (considerLambdasEqualForRedux exprReducedOnce expr) then
    tryReduceToList exprReducedOnce
  else
    (fail "failed to reduce expr to list")


consBif :: LambdaAst -> EitherStringOr LambdaAst
consBif headElem = return (LambdaBif doIt)
  where
    doIt expr = do
      (LambdaList elems) <- tryReduceToList expr
      return (LambdaList (headElem:elems))


applyBif :: LambdaAst -> EitherStringOr LambdaAst
applyBif expr = do
  (LambdaList elems) <- tryReduceToList expr
  return (LambdaApplication elems)


fnBif :: LambdaAst ->  EitherStringOr LambdaAst
fnBif params = return (LambdaBif (\body -> return $ LambdaAbstraction params body))


letinBif :: LambdaAst -> EitherStringOr LambdaAst
letinBif declsExpr = return (LambdaBif doIt)
  where
    doIt body = do
      (LambdaList decls) <- tryReduceToList declsExpr
      case decls of
        [(LambdaList [a, b])] ->
          return (LambdaApplication [(LambdaAbstraction a body), b])
        ((LambdaList [a, b]):rest) ->
          return
            (LambdaApplication 
              [
                (LambdaAbstraction a 
                  (LambdaApplication [(LambdaBif letinBif), (LambdaList rest), body]) 
                ),
                b
              ]
            )
        [a, b] -> 
          return (LambdaApplication [(LambdaAbstraction a body), b])
        _ -> 
          fail $ "letinBif blew up with decls = " ++ show decls
  