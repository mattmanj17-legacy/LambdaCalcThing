{-# OPTIONS_GHC -Wall #-}

module ReduceDebrujin where

import Debrujin

lambdaBetaReducedOneStep :: Debrujin -> Debrujin
lambdaBetaReducedOneStep (DAB val) = 
  (DAB (lambdaBetaReducedOneStep val))
lambdaBetaReducedOneStep (DAP (DAB func) arg) =
  lambdaAppliedTo arg func
lambdaBetaReducedOneStep lap@(DAP func arg)
  | funcReducedOnce /= func = (DAP funcReducedOnce arg)
  | argReducedOnce /= arg = (DAP func argReducedOnce)
  | otherwise = lap
  where
    funcReducedOnce = lambdaBetaReducedOneStep func
    argReducedOnce = lambdaBetaReducedOneStep arg
lambdaBetaReducedOneStep lar@(DAR _) =
  lar

lambdaBetaReducedFull :: Debrujin -> Debrujin
lambdaBetaReducedFull term
  | term == reducedOnce = term
  | otherwise = lambdaBetaReducedFull reducedOnce
  where
    reducedOnce = lambdaBetaReducedOneStep term

lambdaBetaReducedSteps :: [Debrujin] -> Debrujin -> [Debrujin]
lambdaBetaReducedSteps cur term
  | term == reducedOnce = (term:cur)
  | otherwise = lambdaBetaReducedSteps (term:cur) reducedOnce
  where
    reducedOnce = lambdaBetaReducedOneStep term

lambdaAppliedTo :: Debrujin -> Debrujin -> Debrujin
lambdaAppliedTo = lambdaArgRefReplacedWithLambda 1

lambdaArgRefReplacedWithLambda :: Int -> Debrujin -> Debrujin -> Debrujin
lambdaArgRefReplacedWithLambda 1 arg (DAR 1) =
  arg
lambdaArgRefReplacedWithLambda 1 _ (DAR argRef) =
  (DAR (argRef - 1))
lambdaArgRefReplacedWithLambda argRefReplace arg (DAR argRef)
  | argRefReplace == argRef = lambdaIncrementedArgRefsGreaterThan arg 1 argRef
  | otherwise = (DAR argRef)
lambdaArgRefReplacedWithLambda argRefReplace arg (DAB body) =
  (DAB (lambdaArgRefReplacedWithLambda (argRefReplace+1) arg body))
lambdaArgRefReplacedWithLambda argRefReplace argReplace (DAP func arg) =
  (DAP 
    (lambdaArgRefReplacedWithLambda argRefReplace argReplace func) 
    (lambdaArgRefReplacedWithLambda argRefReplace argReplace arg)
  )
  
lambdaIncrementedArgRefsGreaterThan :: Debrujin -> Int -> Int -> Debrujin
lambdaIncrementedArgRefsGreaterThan lar@(DAR argRef) argRefPatchMin argRefReplacing
  | argRef <= argRefPatchMin = lar
  | otherwise = (DAR (argRef + argRefReplacing - 1))
lambdaIncrementedArgRefsGreaterThan (DAB func) argRefPatchMin argRefReplacing =
  (DAB (lambdaIncrementedArgRefsGreaterThan func (argRefPatchMin + 1) argRefReplacing))
lambdaIncrementedArgRefsGreaterThan (DAP func arg) argRefPatchMin argRefReplacing =
  (DAP 
    (lambdaIncrementedArgRefsGreaterThan func argRefPatchMin argRefReplacing)
    (lambdaIncrementedArgRefsGreaterThan arg argRefPatchMin argRefReplacing)
  )