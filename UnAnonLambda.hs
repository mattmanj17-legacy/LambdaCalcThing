{-# OPTIONS_GHC -Wall #-}

module UnAnonLambda 
(
  unAnonLambda
)
where

import Data.Maybe

import LambdaAst
unAnonLambda :: Expr -> Fallible Ast
unAnonLambda = replaceArgRefsWithVars []


incReplacements :: [(Int, String)] -> [(Int, String)]
incReplacements = map ((,) <$> (+1) . fst <*> snd)


varNameFromN :: Int -> String
varNameFromN n = [toEnum (n + (fromEnum 'a'))]


replaceArgRefsWithVars :: [(Int, String)] -> Expr -> Fallible Ast
replaceArgRefsWithVars replacements (ExprArgRef argRef) =
  if isJust replacement then
    return (AstId (fromJust replacement))
  else
    throwE $ "could not replace argref " ++ show argRef
  where
    replacement = lookup argRef replacements

replaceArgRefsWithVars replacements (ExprList elems) = do
  replacedElems <- sequence (map (replaceArgRefsWithVars replacements) elems)
  return (AstList replacedElems)

replaceArgRefsWithVars replacements (ExprAbstraction body) = do
  let nextVarName = varNameFromN $ length replacements
  let nextReplacements = (1, nextVarName):(incReplacements replacements)
  newBody <- replaceArgRefsWithVars nextReplacements body
  return (AstApplication [AstId "fn", AstId nextVarName, newBody])

replaceArgRefsWithVars replacements (ExprApplication func arg) = do
  replacedFunc <- replaceArgRefsWithVars replacements func
  replacedArg <- replaceArgRefsWithVars replacements arg
  return (AstApplication [replacedFunc, replacedArg])