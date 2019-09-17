{-# OPTIONS_GHC -Wall #-}

module DebrujinToLambda 
(
  debrujinToLambda
)
where

import Lambda
import Debrujin

debrujinToLambda :: Debrujin -> Maybe Lambda
debrujinToLambda = replaceArgRefsWithVars []

incReplacements :: [(Int, String)] -> [(Int, String)]
incReplacements = map ((,) <$> (+1) . fst <*> snd)

varNameFromN :: Int -> String
varNameFromN n = [toEnum (n + (fromEnum 'a'))]

replaceArgRefsWithVars :: [(Int, String)] -> Debrujin -> Maybe Lambda
replaceArgRefsWithVars replacements (DAR argRef) = do
  str <- lookup argRef replacements
  return (LAR str)

replaceArgRefsWithVars replacements (DAB body) = do
  let nextVarName = varNameFromN $ length replacements
  let nextReplacements = (1, nextVarName):(incReplacements replacements)
  newBody <- replaceArgRefsWithVars nextReplacements body
  return (LAB [nextVarName] newBody)

replaceArgRefsWithVars replacements (DAP func arg) = do
  newFunc <- replaceArgRefsWithVars replacements func
  newArg <- replaceArgRefsWithVars replacements arg
  return (LAP [newFunc, newArg])