{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module LambdaAst where

import Text.Parsec.Pos

-- Lambda ast, the "sweet" version we parse in

data Ast =
  AstId { srcposAstStart :: SourcePos, srcposAstEnd :: SourcePos, strAstId :: String} |
  AstEmptyList { srcposAstStart :: SourcePos, srcposAstEnd :: SourcePos } |
  AstPair { srcposAstStart :: SourcePos, srcposAstEnd :: SourcePos, astFst :: Ast, astSnd :: Ast } |
  AstApplication { srcposAstStart :: SourcePos, srcposAstEnd :: SourcePos, astFn :: Ast, astArg :: Ast }
  deriving(Eq)

mkAstPair :: Ast -> Ast -> Ast
mkAstPair frst scnd = AstPair (srcposAstStart frst) (srcposAstEnd scnd) frst scnd

mkAstApp :: Ast -> Ast -> Ast
mkAstApp fn arg = AstApplication (srcposAstStart fn) (srcposAstEnd arg) fn arg

instance Show Ast where
  show ast@(AstId {}) = strAstId ast
  show (AstEmptyList {}) = "[]"
  show ast@(AstPair {}) = "<" ++ show (astFst ast) ++ ", " ++ show (astSnd ast) ++ ">"
  show ast@(AstApplication {}) = "(" ++ show (astFn ast) ++ " " ++ show (astArg ast) ++ ")"

-- Lambda Expr, desugared ast, that we can do beta reduction on

data Expr =
  ExprArgRef {argRef :: Int} |
  ExprEmptyList |
  ExprPair {isPairFullyReduced::Bool, exprFst::Expr, exprSnd::Expr} |
  ExprAbstraction {isAbsFullyReduced::Bool, absBody::Expr} |
  ExprApplication {isAppFullyReduced::Bool, exprFn::Expr, exprArg::Expr}
  deriving(Eq)

instance Show Expr where
  show (ExprArgRef {argRef = ar}) = "#" ++ show ar
  show ExprEmptyList = "[]"
  show (ExprPair {isPairFullyReduced = ifr, exprFst = frst, exprSnd = scnd}) = (if ifr then "*" else "") ++ "<" ++ show frst ++ ", " ++ show scnd ++ ">"
  show (ExprAbstraction {isAbsFullyReduced = ifr, absBody = body}) = (if ifr then "*" else "") ++ "(/ " ++ show body ++ ")"
  show (ExprApplication {isAppFullyReduced = ifr, exprFn = fn, exprArg = arg}) = (if ifr then "*" else "") ++ "(" ++ show fn ++ " " ++ show arg ++ ")"