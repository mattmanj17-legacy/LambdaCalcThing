{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module ReduceLambda where

import LambdaAst
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
    start = srcposAstStart ast
    end = srcposAstEnd ast

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
replaceVars reps expr = do
  lift $ tell ["replaceVars in " ++ show expr]
  case expr of
    (AstId {strAstId = idStr}) -> replaceVarsInId reps expr idStr
    (AstPair {astFst = frst, astSnd = scnd}) -> replaceVarsInPair reps (frst, scnd)
    (AstApplication {astFn = fn, astArg = arg}) -> replaceVarsInApp reps (fn, arg)
    AstEmptyList {} -> do
      return ExprEmptyList

replaceVarsInId :: 
  (Monad m) => 
  [(String, Int)] -> 
  Ast -> 
  String -> 
  ExceptT String (WriterT [String] (ReaderT [String] m)) Expr
replaceVarsInId reps astParent str = do
  lift $ tell ["replaceVarsInId " ++ show reps ++ " " ++ show astParent ++ " " ++ show str]
  errStr <- lift $ lift $ errorStrAt astParent ("unrecognized id " ++ str)
  maybe (throwE errStr) (return . ExprArgRef) (lookup str reps)

replaceVarsInPair :: 
  (Monad m) => 
  [(String, Int)] -> 
  (Ast, Ast) -> 
  ExceptT String (WriterT [String] (ReaderT [String] m)) Expr
replaceVarsInPair reps (astFirst, astSecond) = do
  lift $ tell ["replaceVarsInPair " ++ show reps ++ " " ++ show astFirst ++ " " ++ show astSecond]
  newFrst <- replaceVars reps astFirst
  newScnd <- replaceVars reps astSecond
  return $ ExprPair newFrst newScnd

replaceVarsInApp :: 
  (Monad m) =>
  [(String, Int)] -> 
  (Ast, Ast) -> 
  ExceptT String (WriterT [String] (ReaderT [String] m)) Expr
replaceVarsInApp reps (replaceIn, replaceWith) = do
  lift $ tell ["replaceVarsInApp " ++ show reps ++ " " ++ show replaceIn ++ " " ++ show replaceWith]
  case replaceIn of
    (AstApplication {astFn = fn@(AstId {strAstId = "fn"}), astArg = arg}) ->
      replaceVarsInAppFn reps fn (arg, replaceWith)
    _ ->
      replaceVarsInAppDefault reps (replaceIn, replaceWith)

replaceVarsInAppFn :: 
  (Monad m) => 
  [(String, Int)] -> 
  Ast -> 
  (Ast, Ast) -> 
  ExceptT String (WriterT [String] (ReaderT [String] m)) Expr
replaceVarsInAppFn reps fn (params, body) = do
  lift $ tell ["replaceVarsInAppFn " ++ show reps ++ " " ++ show params ++ " " ++ show body]
  case params of
    (AstId {strAstId = idStr}) ->
      replaceVarsInAbsIdParam reps params idStr body
    (AstPair {astFst = frst, astSnd = scnd}) ->
      replaceVarsInAppFnParamsPair reps fn (frst, scnd, body)
    (AstEmptyList {}) -> do
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
    (AstId {strAstId = idStr}) ->
      case paramsScnd of
        AstEmptyList {} ->
          replaceVarsInAbsIdParam reps paramsFrst idStr body
        _ -> 
          replaceVarsInAbsIdParam reps paramsFrst idStr (mkAstApp (mkAstApp fn paramsScnd) body)
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
  return $ ExprApplication fnReplaced argReplaced

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
    return (ExprAbstraction newBody)

-- REDUX

lambdaBetaReducedOneStep :: Expr -> Expr
lambdaBetaReducedOneStep argRef@(ExprArgRef _) =
  argRef

lambdaBetaReducedOneStep (ExprPair frst' scnd') =
  if newFrst == frst' then
    (ExprPair newFrst newScnd)
  else
    (ExprPair newFrst scnd')
  where
    newFrst = lambdaBetaReducedOneStep frst'
    newScnd = lambdaBetaReducedOneStep scnd'

lambdaBetaReducedOneStep (ExprAbstraction val) =
  ExprAbstraction $ lambdaBetaReducedOneStep val

lambdaBetaReducedOneStep (ExprApplication (ExprAbstraction func) arg') =
  lambdaAppliedTo arg' func

lambdaBetaReducedOneStep (ExprApplication func arg') =
  if newFunc == func then
    (ExprApplication func newArg)
  else
    (ExprApplication newFunc arg')
  where
    newFunc = lambdaBetaReducedOneStep func
    newArg = lambdaBetaReducedOneStep arg'

lambdaBetaReducedOneStep ExprEmptyList =
  ExprEmptyList


lambdaBetaReducedFull :: Expr -> Expr
lambdaBetaReducedFull term = 
  if term == reducedOnce then do
    term
  else do
    lambdaBetaReducedFull reducedOnce
  where
    reducedOnce = lambdaBetaReducedOneStep term

lambdaAppliedTo :: Expr -> Expr -> Expr
lambdaAppliedTo = 
  lambdaArgRefReplacedWithLambda 1


lambdaArgRefReplacedWithLambda :: Int -> Expr -> Expr -> Expr
lambdaArgRefReplacedWithLambda argRefReplace arg' (ExprArgRef argRef) = 
  if argRefReplace == argRef then 
    lambdaIncrementedArgRefsGreaterThanOrEqual 1 argRef arg'
  else if argRefReplace < argRef then
    (ExprArgRef (argRef-1))
  else
    (ExprArgRef argRef)

lambdaArgRefReplacedWithLambda argRefReplace arg' (ExprPair frst' scnd') = 
  (ExprPair frstReplaced scndReplaced)
  where
    frstReplaced = lambdaArgRefReplacedWithLambda argRefReplace arg' frst'
    scndReplaced = lambdaArgRefReplacedWithLambda argRefReplace arg' scnd'

lambdaArgRefReplacedWithLambda argRefReplace arg' (ExprAbstraction body) = 
  ExprAbstraction $ lambdaArgRefReplacedWithLambda (argRefReplace+1) arg' body

lambdaArgRefReplacedWithLambda argRefReplace argReplace (ExprApplication func arg') = 
  (ExprApplication funcReplaced argReplaced)
  where
    funcReplaced = lambdaArgRefReplacedWithLambda argRefReplace argReplace func
    argReplaced = lambdaArgRefReplacedWithLambda argRefReplace argReplace arg'

lambdaArgRefReplacedWithLambda _ _ ExprEmptyList = 
  ExprEmptyList
  

lambdaIncrementedArgRefsGreaterThanOrEqual :: Int -> Int -> Expr -> Expr
lambdaIncrementedArgRefsGreaterThanOrEqual argRefPatchMin argRefReplacing lar@(ExprArgRef argRef)
  | argRef < argRefPatchMin = lar
  | otherwise = (ExprArgRef (argRef + argRefReplacing - 1))

lambdaIncrementedArgRefsGreaterThanOrEqual argRefPatchMin argRefReplacing (ExprPair frst' scnd') =
  (ExprPair (incElem frst') (incElem scnd'))
  where
    incElem = lambdaIncrementedArgRefsGreaterThanOrEqual argRefPatchMin argRefReplacing

lambdaIncrementedArgRefsGreaterThanOrEqual argRefPatchMin argRefReplacing (ExprAbstraction body) =
  ExprAbstraction $ lambdaIncrementedArgRefsGreaterThanOrEqual (argRefPatchMin + 1) argRefReplacing body

lambdaIncrementedArgRefsGreaterThanOrEqual argRefPatchMin argRefReplacing (ExprApplication func arg') =
  ExprApplication (incTerm func) (incTerm arg')
  where
    incTerm = lambdaIncrementedArgRefsGreaterThanOrEqual argRefPatchMin argRefReplacing

lambdaIncrementedArgRefsGreaterThanOrEqual _ _ ExprEmptyList =
  ExprEmptyList