{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module ReduceLambda where

import LambdaAst
import Data.Maybe

import Prelude hiding (fail)
import Control.Monad.Fail

-- ANON

anonLambda :: LambdaParsed -> EitherStringOr LambdaCompiled
anonLambda = replaceVars []

incReps :: [(String, Int)] -> [(String, Int)]
incReps = map ((,) <$> fst <*> (+1) . snd)

replaceVars :: [(String, Int)] -> LambdaParsed -> EitherStringOr LambdaCompiled
replaceVars reps repIn =
  case repIn of
    (LambdaParsedId str) -> replaceVarsInId reps str
    (LambdaParsedList elems) -> replaceVarsInList reps elems
    (LambdaParsedApplication terms) -> replaceVarsInApp reps terms

replaceVarsInId :: [(String, Int)] -> String -> EitherStringOr LambdaCompiled
replaceVarsInId reps str =
  maybe (fail $ "could not find " ++ str) (return . LambdaCompiledArgRef) (lookup str reps)

replaceVarsInList :: [(String, Int)] -> [LambdaParsed] -> EitherStringOr LambdaCompiled
replaceVarsInList reps elems = do
  newElems <- sequence $ map (replaceVars reps) elems
  return $ LambdaCompiledList newElems

replaceVarsInApp :: [(String, Int)] -> [LambdaParsed] -> EitherStringOr LambdaCompiled
replaceVarsInApp reps [(LambdaParsedId "fn"), (LambdaParsedId arg), body] =
  replaceVarsInAbsIdParam reps arg body
replaceVarsInApp reps terms = do
  newTerms <- sequence $ map (replaceVars reps) terms
  return $ nestApps newTerms

replaceVarsInAbsIdParam :: [(String, Int)] -> String -> LambdaParsed -> EitherStringOr LambdaCompiled
replaceVarsInAbsIdParam reps str body = do
  if isJust $ lookup str reps then
    fail "replaceVarsInAbsIdParam blew up because we were going to shadow a param"
  else do
    let newreps = (str, 1):(incReps reps)
    newBody <- replaceVars newreps body
    return (LambdaCompiledAbstraction newBody)

nestApps :: [LambdaCompiled] -> LambdaCompiled
nestApps [] = undefined
nestApps [_] = undefined
nestApps [a, b] = LambdaCompiledApplication a b
nestApps (a:b:rest) = nestApps ((nestApps [a, b]):rest)

-- REDUX

lambdasBetaReducedOneStep :: [LambdaCompiled] -> EitherStringOr [LambdaCompiled]
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

lambdaBetaReducedOneStep :: LambdaCompiled -> EitherStringOr LambdaCompiled
lambdaBetaReducedOneStep argRef@(LambdaCompiledArgRef _) =
  return argRef

lambdaBetaReducedOneStep (LambdaCompiledList elems) = do
  elemsReducedOnce <- lambdasBetaReducedOneStep elems
  return (LambdaCompiledList elemsReducedOnce)

lambdaBetaReducedOneStep (LambdaCompiledAbstraction val) = do
  reducedValOnce <- lambdaBetaReducedOneStep val
  return (LambdaCompiledAbstraction reducedValOnce)

lambdaBetaReducedOneStep (LambdaCompiledApplication (LambdaCompiledAbstraction func) arg) =
  lambdaAppliedTo arg func

lambdaBetaReducedOneStep (LambdaCompiledApplication func arg) = do
  [newFunc, newArg] <- lambdasBetaReducedOneStep [func, arg]
  return (LambdaCompiledApplication newFunc newArg)


lambdaBetaReducedFull :: LambdaCompiled -> EitherStringOr LambdaCompiled
lambdaBetaReducedFull term = do
  reducedOnce <- lambdaBetaReducedOneStep term
  if term == reducedOnce then do
    return term
  else do
    lambdaBetaReducedFull reducedOnce

lambdaAppliedTo :: LambdaCompiled -> LambdaCompiled -> EitherStringOr LambdaCompiled
lambdaAppliedTo = 
  lambdaArgRefReplacedWithLambda 1


lambdaArgRefReplacedWithLambda :: Int -> LambdaCompiled -> LambdaCompiled -> EitherStringOr LambdaCompiled
lambdaArgRefReplacedWithLambda argRefReplace arg (LambdaCompiledArgRef argRef) = do
  if argRefReplace == argRef then do
    inced <- lambdaIncrementedArgRefsGreaterThanOrEqual arg 1 argRef
    return inced
  else if argRefReplace < argRef then
    return (LambdaCompiledArgRef (argRef-1))
  else
    return (LambdaCompiledArgRef argRef)

lambdaArgRefReplacedWithLambda argRefReplace arg (LambdaCompiledList elems) = do
  elemsReplaced <- sequence (map (lambdaArgRefReplacedWithLambda argRefReplace arg) elems)
  return (LambdaCompiledList elemsReplaced)

lambdaArgRefReplacedWithLambda argRefReplace arg (LambdaCompiledAbstraction body) = do
  newBody <- lambdaArgRefReplacedWithLambda (argRefReplace+1) arg body
  return (LambdaCompiledAbstraction newBody)

lambdaArgRefReplacedWithLambda argRefReplace argReplace (LambdaCompiledApplication func arg) = do
  funcReplaced <- lambdaArgRefReplacedWithLambda argRefReplace argReplace func
  argReplaced <- lambdaArgRefReplacedWithLambda argRefReplace argReplace arg
  return (LambdaCompiledApplication funcReplaced argReplaced)
  

lambdaIncrementedArgRefsGreaterThanOrEqual :: LambdaCompiled -> Int -> Int -> EitherStringOr LambdaCompiled
lambdaIncrementedArgRefsGreaterThanOrEqual lar@(LambdaCompiledArgRef argRef) argRefPatchMin argRefReplacing
  | argRef < argRefPatchMin = return lar
  | otherwise = return (LambdaCompiledArgRef (argRef + argRefReplacing - 1))

lambdaIncrementedArgRefsGreaterThanOrEqual (LambdaCompiledList elems) argRefPatchMin argRefReplacing = do
  let incElems lElem = lambdaIncrementedArgRefsGreaterThanOrEqual lElem argRefPatchMin argRefReplacing
  incedElems <- sequence (map incElems elems)
  return (LambdaCompiledList incedElems)

lambdaIncrementedArgRefsGreaterThanOrEqual (LambdaCompiledAbstraction body) argRefPatchMin argRefReplacing = do
  incedBody <- lambdaIncrementedArgRefsGreaterThanOrEqual body (argRefPatchMin + 1) argRefReplacing
  return (LambdaCompiledAbstraction incedBody)

lambdaIncrementedArgRefsGreaterThanOrEqual (LambdaCompiledApplication func arg) argRefPatchMin argRefReplacing = do
  let incTerm term = lambdaIncrementedArgRefsGreaterThanOrEqual term argRefPatchMin argRefReplacing
  incedFunc <- incTerm func
  incedArg <- incTerm arg
  return (LambdaCompiledApplication incedFunc incedArg)