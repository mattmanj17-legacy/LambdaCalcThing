{-# OPTIONS_GHC -Wall #-}

module LambdaAst where

import Data.List

data LambdaAst =
  LambdaId String |
  LambdaArgRef Int |
  LambdaList [LambdaAst] |
  LambdaAbstraction LambdaAst LambdaAst |
  LambdaAnonAbstraction LambdaAst |
  LambdaApplication [LambdaAst]
  deriving(Show, Eq)

unParseLambda :: LambdaAst -> String
unParseLambda (LambdaId str) = 
  str
unParseLambda (LambdaAbstraction params body) = 
  "(/ " ++ unParseLambda params ++ " " ++ unParseLambda body ++  ")"
unParseLambda (LambdaAnonAbstraction body) = 
  "(% " ++ unParseLambda body ++  ")"
unParseLambda (LambdaApplication terms) = 
  "(" ++ (intercalate " " $ map unParseLambda terms) ++ ")"
unParseLambda (LambdaList elems) = 
  "[" ++ (intercalate " " $ map unParseLambda elems) ++ "]"
unParseLambda (LambdaArgRef n) = 
  "#" ++ show n 
