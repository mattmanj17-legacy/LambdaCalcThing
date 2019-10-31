{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}

module ReduceLambda where

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Trans.Except
import Data.List
import Data.Maybe

import LambdaAst
import LambdaExpr
import ShowErr

astToExpr :: 
  (Monad m) => 
  AstR -> 
  ExceptT String (WriterT [String] (ReaderT ReplaceVarsEnv m)) Expr
astToExpr = replaceVars

logA :: 
  (MonadTrans t, Monad m) =>
  a -> t (WriterT [a] m) ()
logA = lift . tell . (:[])

data ReplaceVarsEnv = 
  ReplaceVarsEnv 
    { getFileLines :: [String]
    , getReps :: [String]
    }

replaceVars :: 
  (Monad m) => 
  AstR -> 
  ExceptT String (WriterT [String] (ReaderT ReplaceVarsEnv m)) Expr
replaceVars ast = do
  logA $ "replaceVars in " ++ show ast
  case getAst ast of
    AstId astId -> 
      replaceVarsInId ast (getIdStr astId)
    AstPair astPair -> 
      replaceVarsInPair (getFstAst astPair, getSndAst astPair)
    AstApplication astApp -> 
      replaceVarsInApp (getFnAst astApp, getArgAst astApp)
    AstEmptyList -> do
      return ExprEmptyList

replaceVarsInId :: 
  (Monad m) => 
  AstR -> 
  String -> 
  ExceptT String (WriterT [String] (ReaderT ReplaceVarsEnv m)) Expr
replaceVarsInId astParent str = do
  logA $ "replaceVarsInId " ++ show astParent ++ " " ++ show str
  errStr <- lift $ lift $ withReaderT getFileLines $ errorStrAt astParent ("unrecognized id " ++ str)
  reps <- asks getReps
  maybe (throwE errStr) (return . mkExprArgRef . (+1)) (elemIndex str reps)

replaceVarsInPair :: 
  (Monad m) => 
  (AstR, AstR) -> 
  ExceptT String (WriterT [String] (ReaderT ReplaceVarsEnv m)) Expr
replaceVarsInPair (astFirst, astSecond) = do
  logA $ "replaceVarsInPair " ++ show astFirst ++ " " ++ show astSecond
  newFrst <- replaceVars astFirst
  newScnd <- replaceVars astSecond
  return $ mkExprPair newFrst newScnd

replaceVarsInApp :: 
  (Monad m) =>
  (AstR, AstR) -> 
  ExceptT String (WriterT [String] (ReaderT ReplaceVarsEnv m)) Expr
replaceVarsInApp (replaceIn, replaceWith) = do
  logA $ "replaceVarsInApp " ++ show replaceIn ++ " " ++ show replaceWith
  let defaultReplacement = replaceVarsInAppDefault (replaceIn, replaceWith)
  if| (AstApplication astApp) <- getAst replaceIn
    , (AstId astId) <- getAst $ getFnAst astApp ->
      case getIdStr astId of
        "fn" -> 
          replaceVarsInAppFn (getFnAst astApp) ((getArgAst astApp), replaceWith)
        "letin" -> do
          transformed <- 
            ExceptT $ 
            WriterT $ 
            withReaderT getFileLines $ 
            runWriterT $ 
            runExceptT $ 
            transformLetin (getArgAst astApp) replaceWith
          replaceVars transformed
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
  logA $ "transformLetin " ++ show defs ++ " " ++ show body
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
  logA $ "transformLetinPairDefs " ++ show defsFrst ++ " " ++ show defsScnd ++ " " ++ show body
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
  logA $ "transformLetinPairDef " ++ show frstDefId ++ " " ++ show frstDefValue ++ " " ++ show defsScnd ++ " " ++ show body
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
              newSrcInf <- mergeSrcInf (getSrcInf frstDefId) (getSrcInf valFrst)
              appifyAstList
                newSrcInf
                (mkAstIdAt 
                  "fn"
                  (getSrcInf frstDefId)
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
  AstR -> 
  (AstR, AstR) -> 
  ExceptT String (WriterT [String] (ReaderT ReplaceVarsEnv m)) Expr
replaceVarsInAppFn fn (params, body) = do
  logA $ "replaceVarsInAppFn " ++ show params ++ " " ++ show body
  case getAst params of
    AstId astId ->
      replaceVarsInAbsIdParam params idStr body
      where
        idStr = getIdStr astId
    AstPair astPair ->
      replaceVarsInAppFnParamsPair fn (frst, scnd, body)
      where
        frst = getFstAst astPair
        scnd = getSndAst astPair
    AstEmptyList -> do
      errStr <- lift $ lift $ withReaderT getFileLines $ errorStrAt params "empty params list for fn"
      throwE errStr
    _ -> do
      errStr <- lift $ lift $ withReaderT getFileLines $ errorStrAt params ("ill formed params list " ++ show params)
      throwE errStr

replaceVarsInAppFnParamsPair :: 
  (Monad m) =>
  AstR -> 
  (AstR, AstR, AstR) -> 
  ExceptT String (WriterT [String] (ReaderT ReplaceVarsEnv m)) Expr
replaceVarsInAppFnParamsPair fn (paramsFrst, paramsScnd, body) = do
  logA $ "replaceVarsInAppFnParamsPair " ++ show paramsFrst ++ " " ++ show paramsScnd ++ " " ++ show body
  case getAst paramsFrst of
    AstId astId ->
      case getAst paramsScnd of
        AstEmptyList ->
          replaceVarsInAbsIdParam paramsFrst idStr body
        _ -> do 
          app <- mkAstApp fn paramsScnd
          nestedApp <- mkAstApp app body
          replaceVarsInAbsIdParam paramsFrst idStr nestedApp
      where
        idStr = getIdStr astId
    _ -> do
      errStr <- lift $ lift $ withReaderT getFileLines $ errorStrAt paramsFrst "non id in params list for fn"
      throwE errStr 

replaceVarsInAppDefault :: 
  (Monad m) => 
  (AstR, AstR) -> 
  ExceptT String (WriterT [String] (ReaderT ReplaceVarsEnv m)) Expr
replaceVarsInAppDefault (replaceIn, replaceWith) = do
  logA $ "replaceVarsInAppDefault " ++ show replaceIn ++ " " ++ show replaceWith
  fnReplaced <- replaceVars replaceIn
  argReplaced <- replaceVars replaceWith
  return $ mkExprApp fnReplaced argReplaced

prependRepToEnv :: String -> ReplaceVarsEnv -> ReplaceVarsEnv
prependRepToEnv str env =
  ReplaceVarsEnv (getFileLines env) (str:(getReps env))

replaceVarsInAbsIdParam :: 
  (Monad m) =>  
  AstR -> 
  String -> 
  AstR -> 
  ExceptT String (WriterT [String] (ReaderT ReplaceVarsEnv m)) Expr
replaceVarsInAbsIdParam astId str body = do
  logA $ "replaceVarsInAbsIdParam " ++ show astId ++ " " ++ show str ++ " " ++ show body
  reps <- asks getReps
  if isJust $ elemIndex str reps then do
    errStr <- lift $ lift $ withReaderT getFileLines $ errorStrAt astId "replaceVarsInAbsIdParam blew up because we were going to shadow a param"
    throwE errStr 
  else do
    newBody <- local (prependRepToEnv str) $ replaceVars body
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
    