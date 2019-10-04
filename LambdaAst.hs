{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module LambdaAst where

import Text.Parsec.Pos

data Ast =
  AstId { startPos :: SourcePos, endPos :: SourcePos, idStr :: String} |
  AstEmptyList { startPos :: SourcePos, endPos :: SourcePos } |
  AstPair { startPos :: SourcePos, endPos :: SourcePos, frst :: Ast, scnd :: Ast } |
  AstApplication { startPos :: SourcePos, endPos :: SourcePos, fn :: Ast, arg :: Ast }
  deriving(Eq)

instance Show Ast where
  show ast@(AstId {}) = idStr ast
  show (AstEmptyList {}) = "[]"
  show ast@(AstPair {}) = "<" ++ show (frst ast) ++ ", " ++ show (scnd ast) ++ ">"
  show ast@(AstApplication {}) = "(" ++ show (fn ast) ++ " " ++ show (arg ast) ++ ")"

data Expr =
  ExprArgRef Int |
  ExprEmptyList |
  ExprPair Expr Expr |
  ExprAbstraction Expr |
  ExprApplication Expr Expr
  deriving(Show, Eq)