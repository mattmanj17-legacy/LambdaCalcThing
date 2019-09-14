{-# OPTIONS_GHC -Wall #-}

module LambdaToDebrujin where

import Lambda
import Debrujin

lambdaBoundVarsAnonymized :: Lambda -> Maybe Debrujin
lambdaBoundVarsAnonymized = lambdaVarReplacedWithArgRefs []

incReplacements :: [(String, Int)] -> [(String, Int)]
incReplacements = map ((,) <$> fst <*> (+1) . snd)

lambdaVarReplacedWithArgRefs :: [(String, Int)] -> Lambda -> Maybe Debrujin
lambdaVarReplacedWithArgRefs replacements (LAR str) = do
  replacement <- lookup str replacements
  return (DAR replacement)
lambdaVarReplacedWithArgRefs replacements (LAB str body) = do
  let newReplacements = (str, 1):(incReplacements replacements)
  newBody <- lambdaVarReplacedWithArgRefs newReplacements body
  return (DAB newBody)
lambdaVarReplacedWithArgRefs replacements (LAP func arg) = do
  newFunc <- lambdaVarReplacedWithArgRefs replacements func
  newArg <- lambdaVarReplacedWithArgRefs replacements arg
  return (DAP newFunc newArg)