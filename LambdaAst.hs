{-# OPTIONS_GHC -Wall #-}

module LambdaAst where

import Data.List

-- BB should probably try to gen this kind goo with template haskell

data LambdaAstKind =
  KId |
  KArgRef |
  KList |
  KAbstraction |
  KAnonAbstraction |
  KApplication |
  KBif 
  deriving(Show, Eq)

data LambdaAst =
  LambdaId String |
  LambdaArgRef Int |
  LambdaList [LambdaAst] |
  LambdaAbstraction LambdaAst LambdaAst |
  LambdaAnonAbstraction LambdaAst |
  LambdaApplication [LambdaAst] |
  LambdaBif (LambdaAst -> Either String LambdaAst)

kindFromLambdaAst :: LambdaAst -> LambdaAstKind
kindFromLambdaAst (LambdaId _)              = KId
kindFromLambdaAst (LambdaArgRef _)          = KArgRef
kindFromLambdaAst (LambdaList _)            = KList
kindFromLambdaAst (LambdaAbstraction _ _)   = KAbstraction
kindFromLambdaAst (LambdaAnonAbstraction _) = KAnonAbstraction
kindFromLambdaAst (LambdaApplication _)     = KApplication
kindFromLambdaAst (LambdaBif _)             = KBif

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

-- BB should probably move TFN into some common place ...

data TFN =
  TfnTrue |
  TfnFalse |
  TfnNil


tfnAnd :: TFN -> TFN -> TFN
tfnAnd TfnFalse _ = TfnFalse
tfnAnd TfnNil rhs = rhs
tfnAnd TfnTrue TfnNil = TfnTrue
tfnAnd TfnTrue rhs = rhs


tfnOr :: TFN -> TFN -> TFN
tfnOr TfnTrue _ = TfnFalse
tfnOr TfnNil rhs = rhs
tfnOr TfnFalse TfnNil = TfnTrue
tfnOr TfnFalse rhs = rhs


tfnFromBool :: Bool -> TFN
tfnFromBool True = TfnTrue
tfnFromBool False = TfnFalse

boolFromTfn :: Bool -> TFN -> Bool
boolFromTfn treatNilAs TfnNil = treatNilAs
boolFromTfn _ TfnTrue = True
boolFromTfn _ TfnFalse = False


tfnCmpLLambdas :: [LambdaAst] -> [LambdaAst] -> TFN
tfnCmpLLambdas lhs rhs
  | length lhs /= length rhs = TfnFalse
  | otherwise = tfnCmpLLambdaPairs $ zip lhs rhs


tfnCmpLLambdaPairs :: [(LambdaAst, LambdaAst)] -> TFN
tfnCmpLLambdaPairs pairs =
  foldl1 tfnAnd $ map (uncurry tfnCompareLambdas) pairs


tfnCompareLambdas :: LambdaAst -> LambdaAst -> TFN
tfnCompareLambdas (LambdaId str0) (LambdaId str1) = 
  tfnFromBool $ str0 == str1

tfnCompareLambdas (LambdaAbstraction params0 body0) (LambdaAbstraction params1 body1) = 
  tfnAnd (tfnCompareLambdas params0 params1) (tfnCompareLambdas body0 body1)

tfnCompareLambdas (LambdaAnonAbstraction body0) (LambdaAnonAbstraction body1) = 
  tfnCompareLambdas body0 body1

tfnCompareLambdas (LambdaApplication terms0) (LambdaApplication terms1) = 
  tfnCmpLLambdas terms0 terms1

tfnCompareLambdas (LambdaList elems0) (LambdaList elems1) = 
  tfnCmpLLambdas elems0 elems1

tfnCompareLambdas (LambdaArgRef n0) (LambdaArgRef n1) = 
  tfnFromBool $ n0 == n1

tfnCompareLambdas (LambdaBif _) (LambdaBif _) =
  TfnNil

tfnCompareLambdas _ _ =
  TfnFalse