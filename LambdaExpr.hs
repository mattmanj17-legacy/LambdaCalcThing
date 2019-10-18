{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module LambdaExpr where

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