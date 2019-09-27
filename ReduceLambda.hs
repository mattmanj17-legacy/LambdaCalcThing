{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module ReduceLambda where

import LambdaAst
import EitherStringOr
import Data.Maybe

import Prelude hiding (fail)
import Control.Monad.Fail

import Text.Parsec.Pos

errorStrAt :: SourcePos -> SourcePos -> String
errorStrAt startPos endPos = 
  concat
    [ "<"
    , (show (sourceLine startPos))
    , ":"
    , (show (sourceColumn startPos))
    , "-"
    , (show (sourceLine endPos))
    , ":"
    , (show (sourceColumn endPos))
    , ">"
    ]

-- ANON

anonLambda :: RichParsedLambda -> EitherStringOr LambdaCompiled
anonLambda = replaceVars []

incReps :: [(String, Int)] -> [(String, Int)]
incReps = map ((,) <$> fst <*> (+1) . snd)

replaceVars :: [(String, Int)] -> RichParsedLambda -> EitherStringOr LambdaCompiled
replaceVars reps (RichParsedLambda startPos repIn endPos) =
  case repIn of
    (LambdaParsedId str) -> replaceVarsInId reps (startPos, str, endPos)
    (LambdaParsedList elems) -> replaceVarsInList reps elems
    (LambdaParsedApplication terms) -> replaceVarsInApp reps terms

replaceVarsInId :: [(String, Int)] -> (SourcePos, String, SourcePos) -> EitherStringOr LambdaCompiled
replaceVarsInId reps (startStr, str, endStr) =
  maybe (fail $ (errorStrAt startStr endStr) ++ " err1 could not find " ++ str) (return . LambdaCompiledArgRef) (lookup str reps)

replaceVarsInList :: [(String, Int)] -> [RichParsedLambda] -> EitherStringOr LambdaCompiled
replaceVarsInList reps elems = do
  newElems <- sequence $ map (replaceVars reps) elems
  return $ LambdaCompiledList newElems

replaceVarsInApp :: [(String, Int)] -> [RichParsedLambda] -> EitherStringOr LambdaCompiled
replaceVarsInApp 
  reps 
  [ (RichParsedLambda _ (LambdaParsedId "fn") _)
  , (RichParsedLambda startPosArg (LambdaParsedId arg) endPosArg)
  , body
  ] =
  replaceVarsInAbsIdParam reps (startPosArg, arg, endPosArg) body
replaceVarsInApp 
  reps 
  [ (RichParsedLambda fnStart (LambdaParsedId "fn") fnEnd)
  , (RichParsedLambda _ (LambdaParsedList elems) endElems)
  , body
  ] =
  case elems of
    ((RichParsedLambda startPosArg (LambdaParsedId arg) endPosArg):rest) ->
      replaceVarsInAbsIdParam reps (startPosArg, arg, endPosArg) $
        case rest of
          [] -> 
            body
          _ ->
            (RichParsedLambda
              fnStart
              (LambdaParsedApplication
                [(RichParsedLambda fnStart (LambdaParsedId "fn") fnEnd)
                , (RichParsedLambda endPosArg (LambdaParsedList rest) endElems)
                , body
                ]
              )
              endElems
            )
    ((RichParsedLambda startPos _ endPos):_) -> 
      fail $ (errorStrAt startPos endPos) ++ " err4 non id in params list for fn"
    [] ->
      fail $ (errorStrAt fnStart endElems) ++ " err5 empty params list for fn"

replaceVarsInApp
  _
  ((RichParsedLambda startFn (LambdaParsedId "fn") endFn):_) =
  fail $ (errorStrAt startFn endFn) ++ " err3 ill formed fn call"
replaceVarsInApp reps terms = do
  newTerms <- sequence $ map (replaceVars reps) terms
  return $ nestApps newTerms

replaceVarsInAbsIdParam :: [(String, Int)] -> (SourcePos, String, SourcePos) -> RichParsedLambda -> EitherStringOr LambdaCompiled
replaceVarsInAbsIdParam reps (startStr, str, endStr) body = do
  if isJust $ lookup str reps then
    fail $ (errorStrAt startStr endStr) ++ " err2 replaceVarsInAbsIdParam blew up because we were going to shadow a param"
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