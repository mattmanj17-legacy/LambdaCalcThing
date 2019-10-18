{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

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
    start = getStart ast
    end = getEnd ast

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
  case ast of
    AstId {} -> 
      replaceVarsInId reps ast idStr
    AstPair {} -> 
      replaceVarsInPair reps (fstAst, sndAst)
    AstApplication {} -> 
      replaceVarsInApp reps (fnAst, argAst)
    AstEmptyList {} -> do
      return (ExprEmptyList True False)
  where
    idStr = getIdStr ast
    fstAst = getFstAst ast
    sndAst = getSndAst ast
    fnAst = getFnAst ast
    argAst = getArgAst ast

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
  if replaceInMatchesBuiltin "fn" then
    replaceVarsInAppFn reps replaceInFn (replaceInArg, replaceWith)
  else if replaceInMatchesBuiltin "letin" then do
    transformed <- transformLetin replaceInArg replaceWith
    replaceVars reps transformed
  else
    replaceVarsInAppDefault reps (replaceIn, replaceWith)
  where
    replaceInFn = getFnAst replaceIn
    replaceInArg = getArgAst replaceIn
    replaceInFnIdStr = getIdStr replaceInFn
    replaceInMatchesBuiltin strBuiltin =
      isAstApp replaceIn && isAstId replaceInFn && replaceInFnIdStr == strBuiltin

transformLetin ::
  (Monad m) => 
  Ast ->
  Ast ->
  ExceptT String (WriterT [String] (ReaderT [String] m)) Ast
transformLetin defs body = do
  lift $ tell ["transformLetin " ++ show defs ++ " " ++ show body]
  case defs of
    AstId {} -> do
      errStr <- lift $ lift $ errorStrAt defs "letin does not expect an id as its first argument"
      throwE errStr
    AstEmptyList {} ->
      return body
    AstPair {} ->
      transformLetinPairDefs (getFstAst defs) (getSndAst defs) body
    AstApplication {} -> do
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
  case defsFrst of
    AstId {} -> do
      errStr <- lift $ lift $ errorStrAt defsFrst "unexpected id in defs list for letin"
      throwE errStr
    AstEmptyList {} -> do
      errStr <- lift $ lift $ errorStrAt defsFrst "unexpected empty list in defs list for letin"
      throwE errStr
    AstPair {} ->
      transformLetinPairDef (getFstAst defsFrst) (getSndAst defsFrst) defsScnd body
    AstApplication {} -> do
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
  case frstDefId of
    AstId {} -> do
      case frstDefValue of
        AstId {} -> do
          errStr <- lift $ lift $ errorStrAt frstDefValue "unexpected id in def for letin : expected pair"
          throwE errStr
        AstEmptyList {} -> do
          errStr <- lift $ lift $ errorStrAt frstDefValue "unexpected empty list in def for letin : expected pair"
          throwE errStr
        AstPair {} ->
          case valScnd of
            AstEmptyList {} -> do
              transformed <- transformLetin defsScnd body
              return $
                mkAstApp
                  (mkAstApp 
                    (mkAstApp 
                      (AstId 
                        (getStart frstDefId) 
                        (getStart frstDefId) 
                        "fn"
                        False
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
            valFrst = getFstAst frstDefValue
            valScnd = getSndAst frstDefValue
        AstApplication {} -> do
          errStr <- lift $ lift $ errorStrAt frstDefValue "unexpected application in def for letin : expected pair"
          throwE errStr
    AstEmptyList {} -> do
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
  Ast -> 
  (Ast, Ast) -> 
  ExceptT String (WriterT [String] (ReaderT [String] m)) Expr
replaceVarsInAppFn reps fn (params, body) = do
  lift $ tell ["replaceVarsInAppFn " ++ show reps ++ " " ++ show params ++ " " ++ show body]
  case params of
    AstId {} ->
      replaceVarsInAbsIdParam reps params idStr body
      where
        idStr = getIdStr params
    AstPair {} ->
      replaceVarsInAppFnParamsPair reps fn (frst, scnd, body)
      where
        frst = getFstAst params
        scnd = getSndAst params
    AstEmptyList {} -> do
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
  case paramsFrst of
    AstId {} ->
      case paramsScnd of
        AstEmptyList {} ->
          replaceVarsInAbsIdParam reps paramsFrst idStr body
        _ -> 
          replaceVarsInAbsIdParam reps paramsFrst idStr (mkAstApp (mkAstApp fn paramsScnd) body)
      where
        idStr = getIdStr paramsFrst
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