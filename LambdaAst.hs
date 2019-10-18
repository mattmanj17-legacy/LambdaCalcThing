{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module LambdaAst where

import Text.Parsec.Pos

-- Lambda ast, the "sweet" version we parse in

data Ast =
  AstId 
    { getStart :: SourcePos
    , getEnd :: SourcePos
    , getIdStr :: String
    , foo :: Bool
    } |
  AstEmptyList 
    { getStart :: SourcePos
    , getEnd :: SourcePos
    , foo :: Bool 
    } |
  AstPair 
    { getStart :: SourcePos
    , getEnd :: SourcePos
    , getFstAst :: Ast
    , getSndAst :: Ast
    , foo :: Bool 
    } |
  AstApplication 
    { getStart :: SourcePos
    , getEnd :: SourcePos
    , getFnAst :: Ast
    , getArgAst :: Ast
    , foo :: Bool 
    }
  deriving(Eq)

mkAstApp :: Ast -> Ast -> Ast
mkAstApp fn arg = 
  AstApplication start end fn arg False
  where
    start = getStart fn
    end = getEnd arg

isAstId :: Ast -> Bool
isAstId AstId {} = True
isAstId _ = False

isAstApp :: Ast -> Bool
isAstApp AstApplication {} = True
isAstApp _ = False

instance Show Ast where
  show ast =
    case ast of
      AstId {} -> idStr
      AstEmptyList {} -> "[]"
      AstPair {} -> concat ["<", show fstAst, ", ", show sndAst, ">"]
      AstApplication {} -> concat ["(", show fnAst, " ", show argAst, ")"]
    where
      idStr = getIdStr ast
      fstAst = getFstAst ast
      sndAst = getSndAst ast
      fnAst = getFnAst ast
      argAst = getArgAst ast

-- Lambda Expr, desugared ast, that we can do beta reduction on

data Expr =
  ExprArgRef 
    { getIsFullyReduced::Bool
    , getArgRef :: Int
    , goo :: Bool
    } |
  ExprEmptyList 
    { getIsFullyReduced::Bool
    , goo :: Bool
    } |
  ExprPair 
    { getIsFullyReduced::Bool
    , getFstExpr::Expr
    , getSndExpr::Expr
    , goo :: Bool
    } |
  ExprAbstraction 
    { getIsFullyReduced::Bool
    , getBody::Expr
    , getIsLazy :: Bool
    , goo :: Bool
    } |
  ExprApplication 
    { getIsFullyReduced::Bool
    , getFnExpr::Expr
    , getArgExpr::Expr
    , goo :: Bool
    }
  deriving(Eq)

isExprAbstraction :: Expr -> Bool
isExprAbstraction (ExprAbstraction {}) = True
isExprAbstraction _ = False

isExprPair :: Expr -> Bool
isExprPair (ExprPair {}) = True
isExprPair _ = False

isExprEmptyList :: Expr -> Bool
isExprEmptyList (ExprEmptyList {}) = True
isExprEmptyList _ = False

instance Show Expr where
  show expr =
    case expr of
      ExprArgRef {} -> "#" ++ show argRef
      ExprEmptyList {} -> "[]"
      ExprPair {} -> concat [strPrepend, "<", show fstExpr, ", ", show sndExpr, ">"]
      ExprAbstraction {} -> concat [strPrepend, "(/ ", show body, ")"]
      ExprApplication {} -> concat [strPrepend, "(", show fnExpr, " ", show argExpr, ")"]
    where
      argRef = getArgRef expr
      isFullyReduced = getIsFullyReduced expr
      strPrepend = if isFullyReduced then "*" else ""
      fstExpr = getFstExpr expr
      sndExpr = getSndExpr expr
      body = getBody expr
      fnExpr = getFnExpr expr
      argExpr = getArgExpr expr