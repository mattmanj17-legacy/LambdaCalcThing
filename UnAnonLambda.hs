{-# OPTIONS_GHC -Wall #-}

module UnAnonLambda 
(
  unAnonLambda
)
where

import Data.Maybe

import LambdaAst
unAnonLambda :: LambdaCompiled -> EitherStringOr LambdaParsed
unAnonLambda = replaceArgRefsWithVars []


incReplacements :: [(Int, String)] -> [(Int, String)]
incReplacements = map ((,) <$> (+1) . fst <*> snd)


varNameFromN :: Int -> String
varNameFromN n = [toEnum (n + (fromEnum 'a'))]


replaceArgRefsWithVars :: [(Int, String)] -> LambdaCompiled -> EitherStringOr LambdaParsed
replaceArgRefsWithVars replacements (LambdaCompiledArgRef argRef) =
  if isJust replacement then
    return (LambdaParsedId (fromJust replacement))
  else
    fail $ "could not replace argref " ++ show argRef
  where
    replacement = lookup argRef replacements

replaceArgRefsWithVars replacements (LambdaCompiledList elems) = do
  replacedElems <- sequence (map (replaceArgRefsWithVars replacements) elems)
  return (LambdaParsedList replacedElems)

replaceArgRefsWithVars replacements (LambdaCompiledAbstraction body) = do
  let nextVarName = varNameFromN $ length replacements
  let nextReplacements = (1, nextVarName):(incReplacements replacements)
  newBody <- replaceArgRefsWithVars nextReplacements body
  return (LambdaParsedApplication [LambdaParsedId "fn", LambdaParsedId nextVarName, newBody])

replaceArgRefsWithVars replacements (LambdaCompiledApplication func arg) = do
  replacedFunc <- replaceArgRefsWithVars replacements func
  replacedArg <- replaceArgRefsWithVars replacements arg
  return (LambdaParsedApplication [replacedFunc, replacedArg])