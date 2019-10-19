{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE MultiWayIf #-}

module LambdaExpr where

-- Lambda Expr, desugared ast, that we can do beta reduction on

data Expr =
  ExprArgRef ExprArgRefR |
  ExprEmptyList |
  ExprReducible ExprReducibleR
  deriving(Eq)

instance Show Expr where
  show expr =
    case expr of
      ExprArgRef exprArgRef -> show exprArgRef
      ExprEmptyList -> "[]"
      ExprReducible rexpr -> show rexpr

data ExprReducibleR =
  ExprReducibleR
    { getIsReducibleExprFullyReduced::Bool
    , getReducibleExpr::Rexpr
    } 
  deriving(Eq)

instance Show ExprReducibleR where
  show = show . getReducibleExpr

data Rexpr =
  RexprPair RexprPairR |
  RexprAbstraction RexprAbstractionR |
  RexprApplication RexprApplicationR
  deriving(Eq)

instance Show Rexpr where
  show rexpr =
    case rexpr of
      RexprPair rexprPair -> show rexprPair
      RexprAbstraction rexprAbstraction -> show rexprAbstraction
      RexprApplication rexprApplication -> show rexprApplication 

data ExprArgRefR =
  ExprArgRefR 
    { getArgRef :: Int
    } 
  deriving(Eq)

instance Show ExprArgRefR where
  show =  ("#" ++) . show . getArgRef

data RexprPairR =
  RexprPairR 
    { getFstExpr::Expr
    , getSndExpr::Expr
    } 
  deriving(Eq)

instance Show RexprPairR where
  show rexprPair = "<" ++ show (getFstExpr rexprPair) ++ ", " ++ show (getSndExpr rexprPair) ++ ">"

data RexprAbstractionR =
  RexprAbstractionR 
    { getBody::Expr
    } 
  deriving(Eq)

instance Show RexprAbstractionR where
  show rexprAbs = "(/ " ++ show (getBody rexprAbs) ++ ")"

data RexprApplicationR =
  RexprApplicationR 
    { getFnExpr::Expr
    , getArgExpr::Expr
    }
  deriving(Eq)

instance Show RexprApplicationR where
  show rexprApp = "(" ++ show (getFnExpr rexprApp) ++ " " ++ show (getArgExpr rexprApp) ++ ")"

getIsExprFullyReduced :: Expr -> Bool
getIsExprFullyReduced expr =
  case expr of
    ExprArgRef {} -> True
    ExprEmptyList -> True
    ExprReducible rexpr ->
      getIsReducibleExprFullyReduced rexpr

mkExprArgRef :: Int -> Expr
mkExprArgRef = ExprArgRef . ExprArgRefR

mkExprPair :: Expr -> Expr -> Expr
mkExprPair exprFst exprSnd = 
  ExprReducible $ 
  ExprReducibleR ((getIsExprFullyReduced exprFst) && (getIsExprFullyReduced exprSnd)) $ 
  RexprPair $ 
  RexprPairR exprFst exprSnd

mkExprApp :: Expr -> Expr -> Expr
mkExprApp exprFn exprArg = 
  ExprReducible $ 
  ExprReducibleR isFullyReduced $ 
  RexprApplication $ 
  RexprApplicationR exprFn exprArg
  where
    isFullyReduced =
      if| ExprReducible rexpr <- exprFn
        , RexprAbstraction {} <- getReducibleExpr rexpr ->
          False
        | not (getIsExprFullyReduced exprFn) ->
          False
        | not (getIsExprFullyReduced exprArg) ->
          False
        | otherwise -> 
          True

mkExprAbstraction :: Expr -> Expr
mkExprAbstraction exprBody = 
  ExprReducible $ 
  ExprReducibleR (getIsExprFullyReduced exprBody) $ 
  RexprAbstraction $ 
  RexprAbstractionR exprBody
