{-# OPTIONS_GHC -Wall #-}

module ReduceLambda where

import LambdaAst
import Data.Maybe

-- ANON

anonLambda :: LambdaAst -> Maybe LambdaAst
anonLambda = replaceVars []

incReps :: [(String, Int)] -> [(String, Int)]
incReps = map ((,) <$> fst <*> (+1) . snd)

replaceVars :: [(String, Int)] -> LambdaAst -> Maybe LambdaAst
replaceVars reps repIn =
  case repIn of
    (LambdaId str) -> replaceVarsInId reps str
    (LambdaArgRef argRef) -> (Just (LambdaArgRef argRef))
    (LambdaList elems) -> replaceVarsInList reps elems
    (LambdaAbstraction params body) -> replaceVarsInAbs reps params body
    (LambdaAnonAbstraction body) ->  replaceVarsInAnonAbs reps body 
    (LambdaApplication terms) -> replaceVarsInApp reps terms
    (LambdaBif fn) -> (Just (LambdaBif fn))

replaceVarsInId :: [(String, Int)] -> String -> Maybe LambdaAst
replaceVarsInId reps str =
  Just $ maybe (LambdaId str) LambdaArgRef (lookup str reps)

replaceVarsInList :: [(String, Int)] -> [LambdaAst] -> Maybe LambdaAst
replaceVarsInList reps elems = do
  newElems <- sequence $ map (replaceVars reps) elems
  return $ LambdaList newElems

replaceVarsInApp :: [(String, Int)] -> [LambdaAst] -> Maybe LambdaAst
replaceVarsInApp reps terms = do
  newTerms <- sequence $ map (replaceVars reps) terms
  return $ LambdaApplication newTerms

replaceVarsInAnonAbs :: [(String, Int)] -> LambdaAst -> Maybe LambdaAst
replaceVarsInAnonAbs reps body = do
  newBody <- replaceVars (incReps reps) body
  return (LambdaAnonAbstraction newBody)

replaceVarsInAbs :: [(String, Int)] -> LambdaAst -> LambdaAst -> Maybe LambdaAst
replaceVarsInAbs reps params body =
  case params of
    (LambdaId str) -> replaceVarsInAbsIdParam reps str body
    (LambdaArgRef _) -> Nothing
    (LambdaList elems) -> replaceVarsInAbsListParam reps elems body
    (LambdaAbstraction _ _) -> Nothing
    (LambdaAnonAbstraction _) -> Nothing
    (LambdaApplication terms) -> replaceVarsInAbsAppParam reps terms body
    (LambdaBif _) -> Nothing

replaceVarsInAbsIdParam :: [(String, Int)] -> String -> LambdaAst -> Maybe LambdaAst
replaceVarsInAbsIdParam reps str body = do
  if isJust $ lookup str reps then
    Nothing
  else do
    let newreps = (str, 1):(incReps reps)
    newBody <- replaceVars newreps body
    return (LambdaAnonAbstraction newBody)

replaceVarsInAbsListParam :: [(String, Int)] -> [LambdaAst] -> LambdaAst -> Maybe LambdaAst
replaceVarsInAbsListParam reps paramElems body =
  case paramElems of
    [] -> Nothing
    [singleElem] -> replaceVarsInAbs reps singleElem body
    (firstElem:rest) -> replaceVarsInAbs reps firstElem (LambdaAbstraction (LambdaList rest) body)

replaceVarsInAbsAppParam :: [(String, Int)] -> [LambdaAst] -> LambdaAst -> Maybe LambdaAst
replaceVarsInAbsAppParam reps paramTerms body = do
  let params = (LambdaApplication paramTerms)
  paramsReducedOnce <- lambdaBetaReducedOneStep params
  if (params == paramsReducedOnce) then
    Nothing
  else
    replaceVarsInAbs reps paramsReducedOnce body 

-- REDUX

lambdasBetaReducedOneStep :: [LambdaAst] -> Maybe [LambdaAst]
lambdasBetaReducedOneStep [] =
  Just []

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


lambdaBetaReducedOneStep :: LambdaAst -> Maybe LambdaAst
lambdaBetaReducedOneStep (LambdaId "cons") =
  Just (LambdaBif consBif)

lambdaBetaReducedOneStep (LambdaId "apply") =
  Just (LambdaBif applyBif)

lambdaBetaReducedOneStep (LambdaId "fn") =
  Just (LambdaBif fnBif)

lambdaBetaReducedOneStep (LambdaId "letin") =
  Just (LambdaBif letinBif)

lambdaBetaReducedOneStep lid@(LambdaId _) =
  Just lid

lambdaBetaReducedOneStep bif@(LambdaBif _) =
  Just bif

lambdaBetaReducedOneStep argRef@(LambdaArgRef _) =
  Just argRef

lambdaBetaReducedOneStep (LambdaList elems) = do
  elemsReducedOnce <- lambdasBetaReducedOneStep elems
  return (LambdaList elemsReducedOnce)

lambdaBetaReducedOneStep abstraction@(LambdaAbstraction params body) = do
  paramsReducedOnce <- lambdaBetaReducedOneStep params
  if (paramsReducedOnce == params) then do
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
  Nothing

lambdaBetaReducedOneStep (LambdaApplication [_]) =
  Nothing

lambdaBetaReducedOneStep (LambdaApplication terms) = do
  termsReducedOnce <- lambdasBetaReducedOneStep terms
  return (LambdaApplication termsReducedOnce)


lambdaBetaReducedFull :: LambdaAst -> Maybe LambdaAst
lambdaBetaReducedFull term = do
  reducedOnce <- lambdaBetaReducedOneStep term
  if term == reducedOnce then do
    return term
  else do
    lambdaBetaReducedFull reducedOnce

lambdaAppliedTo :: LambdaAst -> LambdaAst -> Maybe LambdaAst
lambdaAppliedTo = 
  lambdaArgRefReplacedWithLambda 1


lambdaArgRefReplacedWithLambda :: Int -> LambdaAst -> LambdaAst -> Maybe LambdaAst
lambdaArgRefReplacedWithLambda _ _ bif@(LambdaBif _) =
  Just bif

lambdaArgRefReplacedWithLambda _ _ lid@(LambdaId _) =
  Just lid

lambdaArgRefReplacedWithLambda argRefReplace arg (LambdaArgRef argRef) = do
  if argRefReplace == argRef then do
    inced <- lambdaIncrementedArgRefsGreaterThanOrEqual arg 1 argRef
    return inced
  else if argRefReplace < argRef then
    Just (LambdaArgRef (argRef-1))
  else
    Just (LambdaArgRef argRef)

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
  

lambdaIncrementedArgRefsGreaterThanOrEqual :: LambdaAst -> Int -> Int -> Maybe LambdaAst
lambdaIncrementedArgRefsGreaterThanOrEqual bif@(LambdaBif _) _ _ =
  Just bif

lambdaIncrementedArgRefsGreaterThanOrEqual lid@(LambdaId _) _ _ =
  Just lid

lambdaIncrementedArgRefsGreaterThanOrEqual lar@(LambdaArgRef argRef) argRefPatchMin argRefReplacing
  | argRef < argRefPatchMin = Just lar
  | otherwise = Just (LambdaArgRef (argRef + argRefReplacing - 1))

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

consBif :: LambdaAst -> Maybe LambdaAst
consBif headElem = Just (LambdaBif doIt)
  where
    doIt (LambdaList elems) = Just (LambdaList (headElem:elems))
    doIt _ = Nothing

applyBif :: LambdaAst -> Maybe LambdaAst
applyBif (LambdaList elems) = Just (LambdaApplication elems)
applyBif _ = Nothing

fnBif :: LambdaAst ->  Maybe LambdaAst
fnBif params = Just (LambdaBif (\body -> Just $ LambdaAbstraction params body))

letinBif :: LambdaAst -> Maybe LambdaAst
letinBif decls =
  Just 
    (LambdaBif
      (\body ->
        case decls of
          (LambdaList ((LambdaList [a, b]):rest)) -> 
            (Just 
              (LambdaApplication 
                [
                  (LambdaAbstraction a 
                    (LambdaApplication [(LambdaBif letinBif), (LambdaList rest), body]) 
                  ),
                  b
                ]
              )
            )
          (LambdaList [a, b]) -> 
            (Just 
              (LambdaApplication [(LambdaAbstraction a body), b])
            )
          _ -> Nothing
      )
    )
  