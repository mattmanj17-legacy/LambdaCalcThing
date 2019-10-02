{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NamedFieldPuns #-}

module LambdaAst where

import Text.Parsec.Pos

data Ast =
  AstId { startPos :: SourcePos, endPos :: SourcePos, idStr :: String} |
  AstEmptyList { startPos :: SourcePos, endPos :: SourcePos } |
  AstPair { startPos :: SourcePos, endPos :: SourcePos, frst :: Ast, scnd :: Ast } |
  AstApplication { startPos :: SourcePos, endPos :: SourcePos, fn :: Ast, arg :: Ast }
  deriving(Eq)

instance Show Ast where
  show (AstId { idStr }) = idStr
  show (AstEmptyList {}) = "[]"
  show (AstPair { frst, scnd }) = "<" ++ show frst ++ ", " ++ show scnd ++ ">"
  show (AstApplication { fn, arg }) = "(" ++ show fn ++ " " ++ show arg ++ ")"

data Expr =
  ExprArgRef Int |
  ExprEmptyList |
  ExprPair Expr Expr |
  ExprAbstraction Expr |
  ExprApplication Expr Expr
  deriving(Show, Eq)