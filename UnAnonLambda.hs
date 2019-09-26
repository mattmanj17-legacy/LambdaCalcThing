{-# OPTIONS_GHC -Wall #-}

module UnAnonLambda 
(
  unAnonLambda
)
where

import Data.Maybe

import LambdaAst
import ReduceLambda

unAnonLambda :: LambdaAst -> EitherStringOr LambdaAst
unAnonLambda = replaceArgRefsWithVars []


incReplacements :: [(Int, String)] -> [(Int, String)]
incReplacements = map ((,) <$> (+1) . fst <*> snd)


varNameFromN :: Int -> String
varNameFromN n = [toEnum (n + (fromEnum 'a'))]


replaceArgRefsWithVars :: [(Int, String)] -> LambdaAst -> EitherStringOr LambdaAst
replaceArgRefsWithVars _ bif@(LambdaBif _) =
  return bif

replaceArgRefsWithVars _ lid@(LambdaId _) =
  return lid

replaceArgRefsWithVars replacements (LambdaArgRef argRef) =
  return $
    if isJust replacement then
      (LambdaId (fromJust replacement))
    else
      (LambdaArgRef argRef)
  where
    replacement = lookup argRef replacements

replaceArgRefsWithVars replacements (LambdaList elems) = do
  replacedElems <- sequence (map (replaceArgRefsWithVars replacements) elems)
  return (LambdaList replacedElems)

replaceArgRefsWithVars replacements abstraction@(LambdaAbstraction _ _) = do
  anoned <- anonLambda abstraction
  replaceArgRefsWithVars replacements anoned

replaceArgRefsWithVars replacements (LambdaAnonAbstraction body) = do
  let nextVarName = varNameFromN $ length replacements
  let nextReplacements = (1, nextVarName):(incReplacements replacements)
  newBody <- replaceArgRefsWithVars nextReplacements body
  return (LambdaAbstraction (LambdaList [LambdaId (nextVarName)]) newBody)

replaceArgRefsWithVars replacements (LambdaApplication terms) = do
  replacedTerms <- sequence (map (replaceArgRefsWithVars replacements) terms)
  return (LambdaApplication replacedTerms)