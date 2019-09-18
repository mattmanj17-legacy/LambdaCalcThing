{-# OPTIONS_GHC -Wall #-}

module ReduceLambda where

import LambdaAst
import Data.Maybe

-- ANON

anonLambda :: LambdaAst -> Either String LambdaAst
anonLambda = replaceVars []

incReps :: [(String, Int)] -> [(String, Int)]
incReps = map ((,) <$> fst <*> (+1) . snd)

replaceVars :: [(String, Int)] -> LambdaAst -> Either String LambdaAst
replaceVars reps repIn =
  case repIn of
    (LambdaId str) -> replaceVarsInId reps str
    (LambdaArgRef argRef) -> (Right (LambdaArgRef argRef))
    (LambdaList elems) -> replaceVarsInList reps elems
    (LambdaAbstraction params body) -> replaceVarsInAbs reps params body
    (LambdaAnonAbstraction body) ->  replaceVarsInAnonAbs reps body 
    (LambdaApplication terms) -> replaceVarsInApp reps terms
    (LambdaBif fn) -> (Right (LambdaBif fn))

replaceVarsInId :: [(String, Int)] -> String -> Either String LambdaAst
replaceVarsInId reps str =
  Right $ maybe (LambdaId str) LambdaArgRef (lookup str reps)

replaceVarsInList :: [(String, Int)] -> [LambdaAst] -> Either String LambdaAst
replaceVarsInList reps elems = do
  newElems <- sequence $ map (replaceVars reps) elems
  return $ LambdaList newElems

replaceVarsInApp :: [(String, Int)] -> [LambdaAst] -> Either String LambdaAst
replaceVarsInApp reps terms = do
  newTerms <- sequence $ map (replaceVars reps) terms
  return $ LambdaApplication newTerms

replaceVarsInAnonAbs :: [(String, Int)] -> LambdaAst -> Either String LambdaAst
replaceVarsInAnonAbs reps body = do
  newBody <- replaceVars (incReps reps) body
  return (LambdaAnonAbstraction newBody)

replaceVarsInAbs :: [(String, Int)] -> LambdaAst -> LambdaAst -> Either String LambdaAst
replaceVarsInAbs reps params body =
  case params of
    (LambdaId str) -> replaceVarsInAbsIdParam reps str body
    (LambdaList elems) -> replaceVarsInAbsListParam reps elems body
    (LambdaApplication terms) -> replaceVarsInAbsAppParam reps terms body
    _ -> Left $ "replaceVarsInAbs blew up on " ++ (show params) ++ " params"

replaceVarsInAbsIdParam :: [(String, Int)] -> String -> LambdaAst -> Either String LambdaAst
replaceVarsInAbsIdParam reps str body = do
  if isJust $ lookup str reps then
    Left "replaceVarsInAbsIdParam blew up because we were going to shadow a param"
  else do
    let newreps = (str, 1):(incReps reps)
    newBody <- replaceVars newreps body
    return (LambdaAnonAbstraction newBody)

replaceVarsInAbsListParam :: [(String, Int)] -> [LambdaAst] -> LambdaAst -> Either String LambdaAst
replaceVarsInAbsListParam reps paramElems body =
  case paramElems of
    [] -> Left "replaceVarsInAbsListParam blew up because we got an empty params list"
    [singleElem] -> replaceVarsInAbs reps singleElem body
    (firstElem:rest) -> replaceVarsInAbs reps firstElem (LambdaAbstraction (LambdaList rest) body)

replaceVarsInAbsAppParam :: [(String, Int)] -> [LambdaAst] -> LambdaAst -> Either String LambdaAst
replaceVarsInAbsAppParam reps paramTerms body = do
  let params = (LambdaApplication paramTerms)
  paramsReducedOnce <- lambdaBetaReducedOneStep params
  if (params == paramsReducedOnce) then
    Left "replaceVarsInAbsAppParam blew up because we got a fully reduced LambdaApplication"
  else
    replaceVarsInAbs reps paramsReducedOnce body 

-- REDUX

lambdasBetaReducedOneStep :: [LambdaAst] -> Either String [LambdaAst]
lambdasBetaReducedOneStep [] =
  Right []

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


lambdaBetaReducedOneStep :: LambdaAst -> Either String LambdaAst
lambdaBetaReducedOneStep (LambdaId "cons") =
  Right (LambdaBif consBif)

lambdaBetaReducedOneStep (LambdaId "apply") =
  Right (LambdaBif applyBif)

lambdaBetaReducedOneStep (LambdaId "fn") =
  Right (LambdaBif fnBif)

lambdaBetaReducedOneStep (LambdaId "letin") =
  Right (LambdaBif letinBif)

lambdaBetaReducedOneStep lid@(LambdaId _) =
  Right lid

lambdaBetaReducedOneStep bif@(LambdaBif _) =
  Right bif

lambdaBetaReducedOneStep argRef@(LambdaArgRef _) =
  Right argRef

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
  Left "lambdaBetaReducedOneStep blew up because we had no terms in a LambdaApplication"

lambdaBetaReducedOneStep (LambdaApplication [_]) =
  Left "lambdaBetaReducedOneStep blew up because we had only one term in a LambdaApplication"

lambdaBetaReducedOneStep (LambdaApplication terms) = do
  termsReducedOnce <- lambdasBetaReducedOneStep terms
  return (LambdaApplication termsReducedOnce)


lambdaBetaReducedFull :: LambdaAst -> Either String LambdaAst
lambdaBetaReducedFull term = do
  reducedOnce <- lambdaBetaReducedOneStep term
  if term == reducedOnce then do
    return term
  else do
    lambdaBetaReducedFull reducedOnce

lambdaAppliedTo :: LambdaAst -> LambdaAst -> Either String LambdaAst
lambdaAppliedTo = 
  lambdaArgRefReplacedWithLambda 1


lambdaArgRefReplacedWithLambda :: Int -> LambdaAst -> LambdaAst -> Either String LambdaAst
lambdaArgRefReplacedWithLambda _ _ bif@(LambdaBif _) =
  Right bif

lambdaArgRefReplacedWithLambda _ _ lid@(LambdaId _) =
  Right lid

lambdaArgRefReplacedWithLambda argRefReplace arg (LambdaArgRef argRef) = do
  if argRefReplace == argRef then do
    inced <- lambdaIncrementedArgRefsGreaterThanOrEqual arg 1 argRef
    return inced
  else if argRefReplace < argRef then
    Right (LambdaArgRef (argRef-1))
  else
    Right (LambdaArgRef argRef)

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
  

lambdaIncrementedArgRefsGreaterThanOrEqual :: LambdaAst -> Int -> Int -> Either String LambdaAst
lambdaIncrementedArgRefsGreaterThanOrEqual bif@(LambdaBif _) _ _ =
  Right bif

lambdaIncrementedArgRefsGreaterThanOrEqual lid@(LambdaId _) _ _ =
  Right lid

lambdaIncrementedArgRefsGreaterThanOrEqual lar@(LambdaArgRef argRef) argRefPatchMin argRefReplacing
  | argRef < argRefPatchMin = Right lar
  | otherwise = Right (LambdaArgRef (argRef + argRefReplacing - 1))

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

consBif :: LambdaAst -> Either String LambdaAst
consBif headElem = Right (LambdaBif doIt)
  where
    doIt (LambdaList elems) = Right (LambdaList (headElem:elems))
    doIt _ = Left "consBif blew up because we tried to cons to somthing that was not a list"

applyBif :: LambdaAst -> Either String LambdaAst
applyBif (LambdaList elems) = Right (LambdaApplication elems)
applyBif _ = Left "applyBif blew up because we tried to 'apply' somthing that was not a list"

fnBif :: LambdaAst ->  Either String LambdaAst
fnBif params = Right (LambdaBif (\body -> Right $ LambdaAbstraction params body))

letinBif :: LambdaAst -> Either String LambdaAst
letinBif decls =
  Right 
    (LambdaBif
      (\body ->
        case decls of
          (LambdaList [(LambdaList [a, b])]) -> 
            (Right 
              (LambdaApplication [(LambdaAbstraction a body), b])
            )
          (LambdaList ((LambdaList [a, b]):rest)) -> 
            (Right 
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
            (Right 
              (LambdaApplication [(LambdaAbstraction a body), b])
            )
          _ -> Left $ "letinBif blew up with decls = " ++ show decls
      )
    )
  