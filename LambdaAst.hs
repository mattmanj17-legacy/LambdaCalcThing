{-# OPTIONS_GHC -Wall #-}

module LambdaAst where

import MetaData

import Text.Parsec.Pos

data AstMetaData = AstMetaData
  { startPos :: SourcePos
  , endPos :: SourcePos
  }
  deriving(Eq)

data Ast =
  AstId String |
  AstEmptyList |
  AstPair (MetaData AstMetaData Ast) (MetaData AstMetaData Ast) |
  AstApplication (MetaData AstMetaData Ast) (MetaData AstMetaData Ast)
  deriving(Eq)

instance Show Ast where
  show (AstId str) = str
  show AstEmptyList = "[]"
  show (AstPair frst scnd) = "<" ++ show (rawData frst) ++ ", " ++ show (rawData scnd) ++ ">"
  show (AstApplication fn arg) = "(" ++ show (rawData fn) ++ " " ++ show (rawData arg) ++ ")"

data Expr =
  ExprArgRef Int |
  ExprEmptyList |
  ExprPair Expr Expr |
  ExprAbstraction Expr |
  ExprApplication Expr Expr
  deriving(Show, Eq)