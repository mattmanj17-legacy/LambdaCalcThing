{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE MultiWayIf #-}

module ReduceLambda where

import LambdaAst
import LambdaExpr
import Control.Monad.Trans.Except
import Data.Maybe

import Text.Parsec.Pos

import Control.Monad.Writer
import Control.Monad.Reader

errorStrAt :: 
  (Monad m) => 
  Ast -> 
  String -> 
  ReaderT [String] m String
errorStrAt ast strMsg = do
  fileLines <- ask
  let viewedLines = take (endLine - startLine + 1) $ (drop (startLine - 1)) fileLines
  let viewedLinesColored = ((mapFirst (insert (startChar - 1) "\x1b[31m")) . (mapLast (insert (endChar - 1) "\x1b[0m"))) viewedLines
  return $ unlines $ posStr:viewedLinesColored
  where
    start = getAstStartPos ast
    end = getAstEndPos ast

    startLine = (sourceLine start)
    startChar = (sourceColumn start)
    endLine = (sourceLine end)
    endChar = (sourceColumn end)

    posStr = 
      concat
        [ "<"
        , (show startLine)
        , ":"
        , (show startChar)
        , "-"
        , (show endLine)
        , ":"
        , (show endChar)
        , "> "
        , strMsg
        ]

    mapFirst _ [] = []
    mapFirst fn (x:xs) = (fn x):xs

    mapLast _ [] = []
    mapLast fn [x] = [fn x]
    mapLast fn (x:xs) = x:(mapLast fn xs)

    insert n xsInsert xsModify = 
      xsStart ++ xsInsert ++ xsEnd
      where
        (xsStart, xsEnd) = splitAt n xsModify

anonLambda :: 
  (Monad m) => 
  Ast -> 
  ExceptT String (WriterT [String] (ReaderT [String] m)) Expr
anonLambda = replaceVars []

incReps :: 
  [(String, Int)] -> 
  [(String, Int)]
incReps = map ((,) <$> fst <*> (+1) . snd)

replaceVars :: 
  (Monad m) =>
  [(String, Int)] -> 
  Ast -> 
  ExceptT String (WriterT [String] (ReaderT [String] m)) Expr
replaceVars reps ast = do
  lift $ tell ["replaceVars in " ++ show ast]
  case getAstNode ast of
    IdNode astId -> 
      replaceVarsInId reps ast (getIdStr astId)
    PairNode astPair -> 
      replaceVarsInPair reps (getFstAst astPair, getSndAst astPair)
    ApplicationNode astApp -> 
      replaceVarsInApp reps (getFnAst astApp, getArgAst astApp)
    EmptyListNode -> do
      return (ExprEmptyList True False)

replaceVarsInId :: 
  (Monad m) => 
  [(String, Int)] -> 
  Ast -> 
  String -> 
  ExceptT String (WriterT [String] (ReaderT [String] m)) Expr
replaceVarsInId reps astParent str = do
  lift $ tell ["replaceVarsInId " ++ show reps ++ " " ++ show astParent ++ " " ++ show str]
  errStr <- lift $ lift $ errorStrAt astParent ("unrecognized id " ++ str)
  maybe (throwE errStr) (return . (\x -> ExprArgRef True x False)) (lookup str reps)

replaceVarsInPair :: 
  (Monad m) => 
  [(String, Int)] -> 
  (Ast, Ast) -> 
  ExceptT String (WriterT [String] (ReaderT [String] m)) Expr
replaceVarsInPair reps (astFirst, astSecond) = do
  lift $ tell ["replaceVarsInPair " ++ show reps ++ " " ++ show astFirst ++ " " ++ show astSecond]
  newFrst <- replaceVars reps astFirst
  newScnd <- replaceVars reps astSecond
  return $ ExprPair False newFrst newScnd False

replaceVarsInApp :: 
  (Monad m) =>
  [(String, Int)] -> 
  (Ast, Ast) -> 
  ExceptT String (WriterT [String] (ReaderT [String] m)) Expr
replaceVarsInApp reps (replaceIn, replaceWith) = do
  lift $ tell ["replaceVarsInApp " ++ show reps ++ " " ++ show replaceIn ++ " " ++ show replaceWith]
  let defaultReplacement = replaceVarsInAppDefault reps (replaceIn, replaceWith)
  if| (ApplicationNode astApp) <- getAstNode replaceIn
    , (IdNode astId) <- getAstNode $ getFnAst astApp ->
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
  Ast ->
  Ast ->
  ExceptT String (WriterT [String] (ReaderT [String] m)) Ast
transformLetin defs body = do
  lift $ tell ["transformLetin " ++ show defs ++ " " ++ show body]
  case getAstNode defs of
    IdNode {} -> do
      errStr <- lift $ lift $ errorStrAt defs "letin does not expect an id as its first argument"
      throwE errStr
    EmptyListNode {} ->
      return body
    PairNode astPair ->
      transformLetinPairDefs (getFstAst astPair) (getSndAst astPair) body
    ApplicationNode {} -> do
      errStr <- lift $ lift $ errorStrAt defs "letin does not expect an application as its first argument"
      throwE errStr

transformLetinPairDefs ::
  (Monad m) => 
  Ast ->
  Ast ->
  Ast ->
  ExceptT String (WriterT [String] (ReaderT [String] m)) Ast
transformLetinPairDefs defsFrst defsScnd body = do
  lift $ tell ["transformLetinPairDefs " ++ show defsFrst ++ " " ++ show defsScnd ++ " " ++ show body]
  case getAstNode defsFrst of
    IdNode {} -> do
      errStr <- lift $ lift $ errorStrAt defsFrst "unexpected id in defs list for letin"
      throwE errStr
    EmptyListNode -> do
      errStr <- lift $ lift $ errorStrAt defsFrst "unexpected empty list in defs list for letin"
      throwE errStr
    PairNode astPair ->
      transformLetinPairDef (getFstAst astPair) (getSndAst astPair) defsScnd body
    ApplicationNode {} -> do
      errStr <- lift $ lift $ errorStrAt defsFrst "unexpected application in defs list for letin"
      throwE errStr

transformLetinPairDef ::
  (Monad m) => 
  Ast ->
  Ast ->
  Ast ->
  Ast ->
  ExceptT String (WriterT [String] (ReaderT [String] m)) Ast
transformLetinPairDef frstDefId frstDefValue defsScnd body = do
  lift $ tell ["transformLetinPairDef " ++ show frstDefId ++ " " ++ show frstDefValue ++ " " ++ show defsScnd ++ " " ++ show body]
  case getAstNode frstDefId of
    IdNode {} -> do
      case getAstNode frstDefValue of
        IdNode {} -> do
          errStr <- lift $ lift $ errorStrAt frstDefValue "unexpected id in def for letin : expected pair"
          throwE errStr
        EmptyListNode -> do
          errStr <- lift $ lift $ errorStrAt frstDefValue "unexpected empty list in def for letin : expected pair"
          throwE errStr
        PairNode astPair ->
          case getAstNode valScnd of
            EmptyListNode -> do
              transformed <- transformLetin defsScnd body
              return $
                mkAstApp
                  (mkAstApp 
                    (mkAstApp 
                      (mkAstIdAt 
                        "fn"
                        (SourceInfo
                          (getAstStartPos frstDefId) 
                          (getAstStartPos frstDefId)
                        )
                      ) 
                      frstDefId
                    ) 
                    transformed
                  )
                  valFrst
            _ -> do
              errStr <- lift $ lift $ errorStrAt valScnd "unexpected value in def for letin : expected empty list"
              throwE errStr
          where
            valFrst = getFstAst astPair
            valScnd = getSndAst astPair
        ApplicationNode {} -> do
          errStr <- lift $ lift $ errorStrAt frstDefValue "unexpected application in def for letin : expected pair"
          throwE errStr
    EmptyListNode -> do
      errStr <- lift $ lift $ errorStrAt frstDefId "unexpected empty list in def for letin : expected id"
      throwE errStr
    PairNode {} -> do
      errStr <- lift $ lift $ errorStrAt frstDefId "unexpected pair in def for letin : expected id"
      throwE errStr
    ApplicationNode {} -> do
      errStr <- lift $ lift $ errorStrAt frstDefId "unexpected application in def for letin : expected id"
      throwE errStr

replaceVarsInAppFn :: 
  (Monad m) => 
  [(String, Int)] -> 
  Ast -> 
  (Ast, Ast) -> 
  ExceptT String (WriterT [String] (ReaderT [String] m)) Expr
replaceVarsInAppFn reps fn (params, body) = do
  lift $ tell ["replaceVarsInAppFn " ++ show reps ++ " " ++ show params ++ " " ++ show body]
  case getAstNode params of
    IdNode astId ->
      replaceVarsInAbsIdParam reps params idStr body
      where
        idStr = getIdStr astId
    PairNode astPair ->
      replaceVarsInAppFnParamsPair reps fn (frst, scnd, body)
      where
        frst = getFstAst astPair
        scnd = getSndAst astPair
    EmptyListNode -> do
      errStr <- lift $ lift $ errorStrAt params "empty params list for fn"
      throwE errStr
    _ -> do
      errStr <- lift $ lift $ errorStrAt params ("ill formed params list " ++ show params)
      throwE errStr

replaceVarsInAppFnParamsPair :: 
  (Monad m) =>
  [(String, Int)] -> 
  Ast -> 
  (Ast, Ast, Ast) -> 
  ExceptT String (WriterT [String] (ReaderT [String] m)) Expr
replaceVarsInAppFnParamsPair reps fn (paramsFrst, paramsScnd, body) = do
  lift $ tell ["replaceVarsInAppFnParamsPair " ++ show reps ++ " " ++ show paramsFrst ++ " " ++ show paramsScnd ++ " " ++ show body]
  case getAstNode paramsFrst of
    IdNode astId ->
      case getAstNode paramsScnd of
        EmptyListNode ->
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
  (Ast, Ast) -> 
  ExceptT String (WriterT [String] (ReaderT [String] m)) Expr
replaceVarsInAppDefault reps (replaceIn, replaceWith) = do
  lift $ tell ["replaceVarsInAppDefault " ++ show reps ++ " " ++ show replaceIn ++ " " ++ show replaceWith]
  fnReplaced <- replaceVars reps replaceIn
  argReplaced <- replaceVars reps replaceWith
  return $ ExprApplication False fnReplaced argReplaced False

replaceVarsInAbsIdParam :: 
  (Monad m) => 
  [(String, Int)] -> 
  Ast -> 
  String -> 
  Ast -> 
  ExceptT String (WriterT [String] (ReaderT [String] m)) Expr
replaceVarsInAbsIdParam reps astId str body = do
  lift $ tell ["replaceVarsInAbsIdParam " ++ show reps ++ " " ++ show astId ++ " " ++ show str ++ " " ++ show body]
  if isJust $ lookup str reps then do
    errStr <- lift $ lift $ errorStrAt astId "replaceVarsInAbsIdParam blew up because we were going to shadow a param"
    throwE errStr 
  else do
    let newreps = (str, 1):(incReps reps)
    newBody <- replaceVars newreps body
    return (ExprAbstraction False newBody True False)

-- REDUX

lambdaBetaReducedOneStep :: Expr -> Expr
lambdaBetaReducedOneStep expr =
  case expr of
    ExprArgRef {} ->
      expr
    ExprPair {} ->
      if getIsFullyReduced frst' then
        (ExprPair (getIsFullyReduced newScnd) frst' newScnd False)
      else
        (ExprPair False newFrst scnd' False)
      where
        frst' = getFstExpr expr
        scnd' = getSndExpr expr
        newFrst = lambdaBetaReducedOneStep frst'
        newScnd = lambdaBetaReducedOneStep scnd'
    ExprAbstraction {} ->
      if getIsFullyReduced val then
        ExprAbstraction True val lazy False
      else
        ExprAbstraction False (lambdaBetaReducedOneStep val) lazy False
      where
        val = getBody expr
        lazy = getIsLazy expr
    ExprApplication {} ->
      if isExprAbstraction func && (funcIsLazy || funcAndArgReduced) then
        lambdaAppliedTo arg funcBody
      else if funcAndArgReduced then
        (ExprApplication True func arg False)
      else if getIsFullyReduced func then
        (ExprApplication False func (lambdaBetaReducedOneStep arg) False)
      else
        (ExprApplication False (lambdaBetaReducedOneStep func) arg False)
      where
        func = getFnExpr expr
        arg = getArgExpr expr
        funcBody = getBody func
        funcAndArgReduced = getIsFullyReduced func && getIsFullyReduced arg
        funcIsLazy = getIsLazy func
    ExprEmptyList {} ->
      expr

lambdaBetaReducedFull :: Expr -> Expr
lambdaBetaReducedFull term = 
  if getIsFullyReduced term then
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
    ExprArgRef {} ->
      if argRefReplace == ar then 
        lambdaIncrementedArgRefsGreaterThanOrEqual 1 ar argReplace
      else if argRefReplace < ar then
        (ExprArgRef True (ar-1) False)
      else
        (ExprArgRef True ar False)
      where
        ar = getArgRef expr
    ExprPair {} ->
      (ExprPair False frstReplaced scndReplaced False)
      where
        frst' = getFstExpr expr
        scnd' = getSndExpr expr
        frstReplaced = lambdaArgRefReplacedWithLambda argRefReplace argReplace frst'
        scndReplaced = lambdaArgRefReplacedWithLambda argRefReplace argReplace scnd'
    ExprAbstraction {} ->
      ExprAbstraction False (lambdaArgRefReplacedWithLambda (argRefReplace+1) argReplace body) lazy False
      where
        body = getBody expr
        lazy = getIsLazy expr
    ExprApplication {} ->
      ExprApplication False funcReplaced argReplaced True
      where
        func = getFnExpr expr
        arg' = getArgExpr expr
        funcReplaced = lambdaArgRefReplacedWithLambda argRefReplace argReplace func
        argReplaced = lambdaArgRefReplacedWithLambda argRefReplace argReplace arg'
    ExprEmptyList {} ->
      expr

lambdaIncrementedArgRefsGreaterThanOrEqual :: Int -> Int -> Expr -> Expr
lambdaIncrementedArgRefsGreaterThanOrEqual argRefPatchMin argRefReplacing expr =
  case expr of
    ExprArgRef {} ->
      if ar < argRefPatchMin then
        expr
      else
        ExprArgRef True (ar + argRefReplacing - 1) False
      where
        ar = getArgRef expr
    ExprPair {} ->
      ExprPair False (incElem frst') (incElem scnd') False
      where
        frst' = getFstExpr expr
        scnd' = getSndExpr expr
        incElem = lambdaIncrementedArgRefsGreaterThanOrEqual argRefPatchMin argRefReplacing
    ExprAbstraction {} ->
      ExprAbstraction False (lambdaIncrementedArgRefsGreaterThanOrEqual (argRefPatchMin + 1) argRefReplacing body) lazy False
      where
        body = getBody expr
        lazy = getIsLazy expr
    ExprApplication {} ->
      ExprApplication False (incTerm func) (incTerm arg') False
      where
        func = getFnExpr expr
        arg' = getArgExpr expr
        incTerm = lambdaIncrementedArgRefsGreaterThanOrEqual argRefPatchMin argRefReplacing
    ExprEmptyList {} ->
      expr