{-# OPTIONS_GHC -Wall #-}

module LambdaToDebrujin 
(
  lambdaBoundVarsAnonymized
)
where

import Lambda
import Debrujin

import Data.Maybe

lambdaBoundVarsAnonymized :: Lambda -> Maybe Debrujin
lambdaBoundVarsAnonymized = lambdaVarReplacedWithArgRefs []

incReplacements :: [(String, Int)] -> [(String, Int)]
incReplacements = map ((,) <$> fst <*> (+1) . snd)

lambdaVarReplacedWithArgRefs :: [(String, Int)] -> Lambda -> Maybe Debrujin
lambdaVarReplacedWithArgRefs replacements (LAR str) = do
  replacement <- lookup str replacements
  return (DAR replacement)

lambdaVarReplacedWithArgRefs _ (LAB [] _) =
  Nothing

lambdaVarReplacedWithArgRefs replacements (LAB [str] body) =
  if isJust (lookup str replacements) then
    Nothing
  else do
    let newReplacements = (str, 1):(incReplacements replacements)
    newBody <- lambdaVarReplacedWithArgRefs newReplacements body
    return (DAB newBody)

lambdaVarReplacedWithArgRefs replacements (LAB (str:strs) body) =
  lambdaVarReplacedWithArgRefs replacements (LAB [str] (LAB strs body))

lambdaVarReplacedWithArgRefs _ (LAP []) =
  Nothing

lambdaVarReplacedWithArgRefs _ (LAP [_]) =
  Nothing

lambdaVarReplacedWithArgRefs replacements (LAP [func, arg]) = do
  newFunc <- lambdaVarReplacedWithArgRefs replacements func
  newArg <- lambdaVarReplacedWithArgRefs replacements arg
  return (DAP newFunc newArg)

lambdaVarReplacedWithArgRefs replacements (LAP (func:arg:rest)) =
  lambdaVarReplacedWithArgRefs replacements (LAP ((LAP [func, arg]):rest))