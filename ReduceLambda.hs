{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE MultiWayIf #-}

module ReduceLambda where

import LambdaAst
import LambdaExpr
import Control.Monad.Trans.Except
import Data.Maybe

import Control.Monad.Writer
import Control.Monad.Reader

import ShowErr

anonLambda :: 
  (Monad m) => 
  AstR -> 
  ExceptT String (WriterT [String] (ReaderT [String] m)) Expr
anonLambda = replaceVars []

incReps :: 
  [(String, Int)] -> 
  [(String, Int)]
incReps = map ((,) <$> fst <*> (+1) . snd)

replaceVars :: 
  (Monad m) =>
  [(String, Int)] -> 
  AstR -> 
  ExceptT String (WriterT [String] (ReaderT [String] m)) Expr
replaceVars reps ast = do
  lift $ tell ["replaceVars in " ++ show ast]
  case getAst ast of
    AstId astId -> 
      replaceVarsInId reps ast (getIdStr astId)
    AstPair astPair -> 
      replaceVarsInPair reps (getFstAst astPair, getSndAst astPair)
    AstApplication astApp -> 
      replaceVarsInApp reps (getFnAst astApp, getArgAst astApp)
    AstEmptyList -> do
      return ExprEmptyList

replaceVarsInId :: 
  (Monad m) => 
  [(String, Int)] -> 
  AstR -> 
  String -> 
  ExceptT String (WriterT [String] (ReaderT [String] m)) Expr
replaceVarsInId reps astParent str = do
  lift $ tell ["replaceVarsInId " ++ show reps ++ " " ++ show astParent ++ " " ++ show str]
  errStr <- lift $ lift $ errorStrAt astParent ("unrecognized id " ++ str)
  maybe (throwE errStr) (return . mkExprArgRef) (lookup str reps)

replaceVarsInPair :: 
  (Monad m) => 
  [(String, Int)] -> 
  (AstR, AstR) -> 
  ExceptT String (WriterT [String] (ReaderT [String] m)) Expr
replaceVarsInPair reps (astFirst, astSecond) = do
  lift $ tell ["replaceVarsInPair " ++ show reps ++ " " ++ show astFirst ++ " " ++ show astSecond]
  newFrst <- replaceVars reps astFirst
  newScnd <- replaceVars reps astSecond
  return $ mkExprPair newFrst newScnd

replaceVarsInApp :: 
  (Monad m) =>
  [(String, Int)] -> 
  (AstR, AstR) -> 
  ExceptT String (WriterT [String] (ReaderT [String] m)) Expr
replaceVarsInApp reps (replaceIn, replaceWith) = do
  lift $ tell ["replaceVarsInApp " ++ show reps ++ " " ++ show replaceIn ++ " " ++ show replaceWith]
  let defaultReplacement = replaceVarsInAppDefault reps (replaceIn, replaceWith)
  if| (AstApplication astApp) <- getAst replaceIn
    , (AstId astId) <- getAst $ getFnAst astApp ->
      case getIdStr astId of
        "fn" -> 
          replaceVarsInAppFn reps (getFnAst astApp) ((getArgAst astApp), replaceWith)
        "letin" -> do
          transformed <- transformLetin (getArgAst astApp) replaceWith
          replaceVars reps transformed
        _ ->
          defaultReplacement
    | otherwise -> 
      defaultReplacement

transformLetin ::
  (Monad m) => 
  AstR ->
  AstR ->
  ExceptT String (WriterT [String] (ReaderT [String] m)) AstR
transformLetin defs body = do
  lift $ tell ["transformLetin " ++ show defs ++ " " ++ show body]
  case getAst defs of
    AstId {} -> do
      errStr <- lift $ lift $ errorStrAt defs "letin does not expect an id as its first argument"
      throwE errStr
    AstEmptyList {} ->
      return body
    AstPair astPair ->
      transformLetinPairDefs (getFstAst astPair) (getSndAst astPair) body
    AstApplication {} -> do
      errStr <- lift $ lift $ errorStrAt defs "letin does not expect an application as its first argument"
      throwE errStr

transformLetinPairDefs ::
  (Monad m) => 
  AstR ->
  AstR ->
  AstR ->
  ExceptT String (WriterT [String] (ReaderT [String] m)) AstR
transformLetinPairDefs defsFrst defsScnd body = do
  lift $ tell ["transformLetinPairDefs " ++ show defsFrst ++ " " ++ show defsScnd ++ " " ++ show body]
  case getAst defsFrst of
    AstId {} -> do
      errStr <- lift $ lift $ errorStrAt defsFrst "unexpected id in defs list for letin"
      throwE errStr
    AstEmptyList -> do
      errStr <- lift $ lift $ errorStrAt defsFrst "unexpected empty list in defs list for letin"
      throwE errStr
    AstPair astPair ->
      transformLetinPairDef (getFstAst astPair) (getSndAst astPair) defsScnd body
    AstApplication {} -> do
      errStr <- lift $ lift $ errorStrAt defsFrst "unexpected application in defs list for letin"
      throwE errStr

transformLetinPairDef ::
  (Monad m) => 
  AstR ->
  AstR ->
  AstR ->
  AstR ->
  ExceptT String (WriterT [String] (ReaderT [String] m)) AstR
transformLetinPairDef frstDefId frstDefValue defsScnd body = do
  lift $ tell ["transformLetinPairDef " ++ show frstDefId ++ " " ++ show frstDefValue ++ " " ++ show defsScnd ++ " " ++ show body]
  case getAst frstDefId of
    AstId {} -> do
      case getAst frstDefValue of
        AstId {} -> do
          errStr <- lift $ lift $ errorStrAt frstDefValue "unexpected id in def for letin : expected pair"
          throwE errStr
        AstEmptyList -> do
          errStr <- lift $ lift $ errorStrAt frstDefValue "unexpected empty list in def for letin : expected pair"
          throwE errStr
        AstPair astPair ->
          case getAst valScnd of
            AstEmptyList -> do
              transformed <- transformLetin defsScnd body
              return $
                appifyAstList
                  (SourceInfo
                    (getAstStartPos frstDefId)
                    (getAstEndPos valFrst)
                  )
                  (mkAstIdAt 
                    "fn"
                    (SourceInfo
                      (getAstStartPos frstDefId) 
                      (getAstStartPos frstDefId)
                    )
                  )
                  [ frstDefId
                  , transformed
                  , valFrst
                  ]
            _ -> do
              errStr <- lift $ lift $ errorStrAt valScnd "unexpected value in def for letin : expected empty list"
              throwE errStr
          where
            valFrst = getFstAst astPair
            valScnd = getSndAst astPair
        AstApplication {} -> do
          errStr <- lift $ lift $ errorStrAt frstDefValue "unexpected application in def for letin : expected pair"
          throwE errStr
    AstEmptyList -> do
      errStr <- lift $ lift $ errorStrAt frstDefId "unexpected empty list in def for letin : expected id"
      throwE errStr
    AstPair {} -> do
      errStr <- lift $ lift $ errorStrAt frstDefId "unexpected pair in def for letin : expected id"
      throwE errStr
    AstApplication {} -> do
      errStr <- lift $ lift $ errorStrAt frstDefId "unexpected application in def for letin : expected id"
      throwE errStr

replaceVarsInAppFn :: 
  (Monad m) => 
  [(String, Int)] -> 
  AstR -> 
  (AstR, AstR) -> 
  ExceptT String (WriterT [String] (ReaderT [String] m)) Expr
replaceVarsInAppFn reps fn (params, body) = do
  lift $ tell ["replaceVarsInAppFn " ++ show reps ++ " " ++ show params ++ " " ++ show body]
  case getAst params of
    AstId astId ->
      replaceVarsInAbsIdParam reps params idStr body
      where
        idStr = getIdStr astId
    AstPair astPair ->
      replaceVarsInAppFnParamsPair reps fn (frst, scnd, body)
      where
        frst = getFstAst astPair
        scnd = getSndAst astPair
    AstEmptyList -> do
      errStr <- lift $ lift $ errorStrAt params "empty params list for fn"
      throwE errStr
    _ -> do
      errStr <- lift $ lift $ errorStrAt params ("ill formed params list " ++ show params)
      throwE errStr

replaceVarsInAppFnParamsPair :: 
  (Monad m) =>
  [(String, Int)] -> 
  AstR -> 
  (AstR, AstR, AstR) -> 
  ExceptT String (WriterT [String] (ReaderT [String] m)) Expr
replaceVarsInAppFnParamsPair reps fn (paramsFrst, paramsScnd, body) = do
  lift $ tell ["replaceVarsInAppFnParamsPair " ++ show reps ++ " " ++ show paramsFrst ++ " " ++ show paramsScnd ++ " " ++ show body]
  case getAst paramsFrst of
    AstId astId ->
      case getAst paramsScnd of
        AstEmptyList ->
          replaceVarsInAbsIdParam reps paramsFrst idStr body
        _ -> 
          replaceVarsInAbsIdParam reps paramsFrst idStr (mkAstApp (mkAstApp fn paramsScnd) body)
      where
        idStr = getIdStr astId
    _ -> do
      errStr <- lift $ lift $ errorStrAt paramsFrst "non id in params list for fn"
      throwE errStr 

replaceVarsInAppDefault :: 
  (Monad m) => 
  [(String, Int)] -> 
  (AstR, AstR) -> 
  ExceptT String (WriterT [String] (ReaderT [String] m)) Expr
replaceVarsInAppDefault reps (replaceIn, replaceWith) = do
  lift $ tell ["replaceVarsInAppDefault " ++ show reps ++ " " ++ show replaceIn ++ " " ++ show replaceWith]
  fnReplaced <- replaceVars reps replaceIn
  argReplaced <- replaceVars reps replaceWith
  return $ mkExprApp fnReplaced argReplaced

replaceVarsInAbsIdParam :: 
  (Monad m) => 
  [(String, Int)] -> 
  AstR -> 
  String -> 
  AstR -> 
  ExceptT String (WriterT [String] (ReaderT [String] m)) Expr
replaceVarsInAbsIdParam reps astId str body = do
  lift $ tell ["replaceVarsInAbsIdParam " ++ show reps ++ " " ++ show astId ++ " " ++ show str ++ " " ++ show body]
  if isJust $ lookup str reps then do
    errStr <- lift $ lift $ errorStrAt astId "replaceVarsInAbsIdParam blew up because we were going to shadow a param"
    throwE errStr 
  else do
    let newreps = (str, 1):(incReps reps)
    newBody <- replaceVars newreps body
    return (mkExprAbstraction newBody)

-- REDUX

lambdaBetaReducedOneStep :: Expr -> Expr
lambdaBetaReducedOneStep exprNode =
  case exprNode of
    ExprArgRef {} ->
      exprNode
    ExprEmptyList ->
      exprNode
    ExprReducible rexper ->
      case getReducibleExpr rexper of
        RexprPair rexprPair ->
          if getIsExprFullyReduced frst' then
            mkExprPair frst' newScnd
          else
            mkExprPair newFrst scnd'
          where
            frst' = getFstExpr rexprPair
            scnd' = getSndExpr rexprPair
            newFrst = lambdaBetaReducedOneStep frst'
            newScnd = lambdaBetaReducedOneStep scnd'
        RexprAbstraction rexprAbs ->
          mkExprAbstraction (lambdaBetaReducedOneStep (getBody rexprAbs))
        RexprApplication rexprApp ->
          if| ExprReducible rexprFunc <- func
            , RexprAbstraction rexprAbsFunc <- getReducibleExpr rexprFunc ->
              lambdaAppliedTo arg (getBody rexprAbsFunc)
            | getIsExprFullyReduced func ->
              mkExprApp func (lambdaBetaReducedOneStep arg)
            | otherwise ->
              mkExprApp (lambdaBetaReducedOneStep func) arg
          where
            func = getFnExpr rexprApp
            arg = getArgExpr rexprApp
    
lambdaBetaReducedFull :: Expr -> Expr
lambdaBetaReducedFull term = 
  if getIsExprFullyReduced term then
    term
  else
    lambdaBetaReducedFull reducedOnce
  where
    reducedOnce = lambdaBetaReducedOneStep term

lambdaAppliedTo :: Expr -> Expr -> Expr
lambdaAppliedTo = 
  lambdaArgRefReplacedWithLambda 1

lambdaArgRefReplacedWithLambda :: Int -> Expr -> Expr -> Expr
lambdaArgRefReplacedWithLambda argRefReplace argReplace expr =
  case expr of
    ExprArgRef exprArgRef ->
      if argRefReplace == ar then 
        lambdaIncrementedArgRefsGreaterThanOrEqual 1 ar argReplace
      else if argRefReplace < ar then
        mkExprArgRef (ar-1)
      else
        mkExprArgRef ar
      where
        ar = getArgRef exprArgRef
    ExprEmptyList ->
      expr
    ExprReducible rexpr ->
      case getReducibleExpr rexpr of
        RexprPair rexprPair ->
          mkExprPair frstReplaced scndReplaced
          where
            frst' = getFstExpr rexprPair
            scnd' = getSndExpr rexprPair
            frstReplaced = lambdaArgRefReplacedWithLambda argRefReplace argReplace frst'
            scndReplaced = lambdaArgRefReplacedWithLambda argRefReplace argReplace scnd'
        RexprAbstraction rexprAbs ->
          mkExprAbstraction 
            (lambdaArgRefReplacedWithLambda 
              (argRefReplace+1) 
              argReplace 
              (getBody rexprAbs)
            )
        RexprApplication rexprApp ->
          mkExprApp funcReplaced argReplaced
          where
            func = getFnExpr rexprApp
            arg' = getArgExpr rexprApp
            funcReplaced = lambdaArgRefReplacedWithLambda argRefReplace argReplace func
            argReplaced = lambdaArgRefReplacedWithLambda argRefReplace argReplace arg'
    
lambdaIncrementedArgRefsGreaterThanOrEqual :: Int -> Int -> Expr -> Expr
lambdaIncrementedArgRefsGreaterThanOrEqual argRefPatchMin argRefReplacing expr =
  case expr of
    ExprArgRef exprArgRef ->
      if ar < argRefPatchMin then
        expr
      else
        mkExprArgRef (ar + argRefReplacing - 1)
      where
        ar = getArgRef exprArgRef
    ExprEmptyList ->
      expr
    ExprReducible rexpr ->
      case getReducibleExpr rexpr of
        RexprPair rexprPair ->
          mkExprPair (incElem frst') (incElem scnd')
          where
            frst' = getFstExpr rexprPair
            scnd' = getSndExpr rexprPair
            incElem = lambdaIncrementedArgRefsGreaterThanOrEqual argRefPatchMin argRefReplacing
        RexprAbstraction rexprAbs ->
          mkExprAbstraction 
            (lambdaIncrementedArgRefsGreaterThanOrEqual 
              (argRefPatchMin + 1) 
              argRefReplacing 
              (getBody rexprAbs)
            )
        RexprApplication rexprApp ->
          mkExprApp (incTerm func) (incTerm arg')
          where
            func = getFnExpr rexprApp
            arg' = getArgExpr rexprApp
            incTerm = lambdaIncrementedArgRefsGreaterThanOrEqual argRefPatchMin argRefReplacing
    