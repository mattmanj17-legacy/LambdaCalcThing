{-# OPTIONS_GHC -Wall #-}

module LambdaAst where

import Data.List

data LambdaAst =
  LambdaId String |
  LambdaArgRef Int |
  LambdaList [LambdaAst] |
  LambdaAbstraction LambdaAst LambdaAst |
  LambdaAnonAbstraction LambdaAst |
  LambdaApplication [LambdaAst] |
  LambdaBif (LambdaAst -> Maybe LambdaAst)

instance Show LambdaAst where
  show (LambdaId str) = 
    str
  show (LambdaAbstraction params body) = 
    "(/ " ++ show params ++ " " ++ show body ++  ")"
  show (LambdaAnonAbstraction body) = 
    "(% " ++ show body ++  ")"
  show (LambdaApplication terms) = 
    "(" ++ (intercalate " " $ map show terms) ++ ")"
  show (LambdaList elems) = 
    "[" ++ (intercalate " " $ map show elems) ++ "]"
  show (LambdaArgRef n) = 
    "#" ++ show n 
  show (LambdaBif _) = 
    "<bif>"

instance Eq LambdaAst where
  (==) (LambdaId str0) (LambdaId str1) = str0 == str1
  (==) (LambdaAbstraction params0 body0) (LambdaAbstraction params1 body1) = params0 == params1 && body0 == body1
  (==) (LambdaAnonAbstraction body0) (LambdaAnonAbstraction body1) = body0 == body1
  (==) (LambdaApplication terms0) (LambdaApplication terms1) = terms0 == terms1
  (==) (LambdaList elems0) (LambdaList elems1) = elems0 == elems1
  (==) (LambdaArgRef n0) (LambdaArgRef n1) = n0 == n1
  (==) _ _ = False -- ugh, this includes bifs