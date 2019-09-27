{-# OPTIONS_GHC -Wall #-}

module LambdaAst where

import Text.Parsec.Pos

data LambdaParsed =
  LambdaParsedId String |
  LambdaParsedList [RichParsedLambda] |
  LambdaParsedApplication [RichParsedLambda]
  deriving(Show, Eq)

data RichParsedLambda = 
  RichParsedLambda SourcePos LambdaParsed SourcePos
  deriving(Show, Eq)

data LambdaCompiled =
  LambdaCompiledArgRef Int |
  LambdaCompiledList [LambdaCompiled] |
  LambdaCompiledAbstraction LambdaCompiled |
  LambdaCompiledApplication LambdaCompiled LambdaCompiled
  deriving(Show, Eq)