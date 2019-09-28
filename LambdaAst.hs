{-# OPTIONS_GHC -Wall #-}

module LambdaAst where

import MetaData

import Text.Parsec.Pos

data AstMetaData = AstMetaData
  { startPos :: SourcePos
  , endPos :: SourcePos
  }
  deriving(Show, Eq)

data Ast =
  AstId String |
  AstList [MetaData AstMetaData Ast] |
  AstApplication [MetaData AstMetaData Ast]
  deriving(Show, Eq)

data Expr =
  ExprArgRef Int |
  ExprList [Expr] |
  ExprAbstraction Expr |
  ExprApplication Expr Expr
  deriving(Show, Eq)