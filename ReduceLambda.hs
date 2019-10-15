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
  return $ ExprPair False newFrst newScnd

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
    (AstApplication {astFn = (AstId {strAstId = "letin"}), astArg = arg}) -> do
      transformed <- transformLetin arg replaceWith
      replaceVars reps transformed
    _ ->
      replaceVarsInAppDefault reps (replaceIn, replaceWith)

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
    AstPair {astFst = frst, astSnd = scnd} ->
      transformLetinPairDefs frst scnd body
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
    AstPair {astFst = frstDefId, astSnd = frstDefValue} ->
      transformLetinPairDef frstDefId frstDefValue defsScnd body
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
        AstPair {astFst = valFrst, astSnd = valScnd} ->
          case valScnd of
            AstEmptyList {} -> do
              transformed <- transformLetin defsScnd body
              return $
                mkAstApp
                  (mkAstApp 
                    (mkAstApp 
                      (AstId 
                        (srcposAstStart frstDefId) 
                        (srcposAstStart frstDefId) 
                        "fn"
                      ) 
                      frstDefId
                    ) 
                    transformed
                  )
                  valFrst
            _ -> do
              errStr <- lift $ lift $ errorStrAt valScnd "unexpected value in def for letin : expected empty list"
              throwE errStr
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
  return $ ExprApplication False fnReplaced argReplaced

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
    return (ExprAbstraction False newBody True)

-- REDUX

isFullyReduced :: Expr -> Bool
isFullyReduced expr =
  case expr of
    ExprArgRef {} -> True
    ExprEmptyList {} -> True
    ExprPair {isPairFullyReduced = reduced} -> reduced
    ExprAbstraction {isAbsFullyReduced = reduced} -> reduced
    ExprApplication {isAppFullyReduced = reduced} -> reduced

lambdaBetaReducedOneStep :: Expr -> Expr
lambdaBetaReducedOneStep ar@(ExprArgRef _) =
  ar

lambdaBetaReducedOneStep (ExprPair _ frst' scnd') =
  if isFullyReduced frst' then
    (ExprPair (isFullyReduced newScnd) frst' newScnd)
  else
    (ExprPair False newFrst scnd')
  where
    newFrst = lambdaBetaReducedOneStep frst'
    newScnd = lambdaBetaReducedOneStep scnd'

lambdaBetaReducedOneStep (ExprAbstraction _ val lazy) =
  if isFullyReduced val then
    ExprAbstraction True val lazy
  else
    ExprAbstraction False (lambdaBetaReducedOneStep val) lazy

lambdaBetaReducedOneStep app@(ExprApplication {}) =
  if isAbstraction func && (funcIsLazy || funcAndArgReduced) then
    lambdaAppliedTo arg funcBody
  else if funcAndArgReduced then
    (ExprApplication True func arg)
  else if isFullyReduced func then
    (ExprApplication False func (lambdaBetaReducedOneStep arg))
  else
    (ExprApplication False (lambdaBetaReducedOneStep func) arg)
  where
    func = exprFn app
    arg = exprArg app
    funcBody = absBody func
    funcAndArgReduced = isFullyReduced func && isFullyReduced arg
    funcIsLazy = isLazy func

lambdaBetaReducedOneStep ExprEmptyList =
  ExprEmptyList


lambdaBetaReducedFull :: Expr -> Expr
lambdaBetaReducedFull term = 
  if isFullyReduced term then
    term
  else
    lambdaBetaReducedFull reducedOnce
  where
    reducedOnce = lambdaBetaReducedOneStep term

lambdaAppliedTo :: Expr -> Expr -> Expr
lambdaAppliedTo = 
  lambdaArgRefReplacedWithLambda 1


lambdaArgRefReplacedWithLambda :: Int -> Expr -> Expr -> Expr
lambdaArgRefReplacedWithLambda argRefReplace arg' (ExprArgRef ar) = 
  if argRefReplace == ar then 
    lambdaIncrementedArgRefsGreaterThanOrEqual 1 ar arg'
  else if argRefReplace < ar then
    (ExprArgRef (ar-1))
  else
    (ExprArgRef ar)

lambdaArgRefReplacedWithLambda argRefReplace arg' (ExprPair _ frst' scnd') = 
  (ExprPair False frstReplaced scndReplaced)
  where
    frstReplaced = lambdaArgRefReplacedWithLambda argRefReplace arg' frst'
    scndReplaced = lambdaArgRefReplacedWithLambda argRefReplace arg' scnd'

lambdaArgRefReplacedWithLambda argRefReplace arg' (ExprAbstraction _ body lazy) = 
  ExprAbstraction False (lambdaArgRefReplacedWithLambda (argRefReplace+1) arg' body) lazy

lambdaArgRefReplacedWithLambda argRefReplace argReplace (ExprApplication _ func arg') = 
  (ExprApplication False funcReplaced argReplaced)
  where
    funcReplaced = lambdaArgRefReplacedWithLambda argRefReplace argReplace func
    argReplaced = lambdaArgRefReplacedWithLambda argRefReplace argReplace arg'

lambdaArgRefReplacedWithLambda _ _ ExprEmptyList = 
  ExprEmptyList
  

lambdaIncrementedArgRefsGreaterThanOrEqual :: Int -> Int -> Expr -> Expr
lambdaIncrementedArgRefsGreaterThanOrEqual argRefPatchMin argRefReplacing lar@(ExprArgRef ar)
  | ar < argRefPatchMin = lar
  | otherwise = (ExprArgRef (ar + argRefReplacing - 1))

lambdaIncrementedArgRefsGreaterThanOrEqual argRefPatchMin argRefReplacing (ExprPair _ frst' scnd') =
  (ExprPair False (incElem frst') (incElem scnd'))
  where
    incElem = lambdaIncrementedArgRefsGreaterThanOrEqual argRefPatchMin argRefReplacing

lambdaIncrementedArgRefsGreaterThanOrEqual argRefPatchMin argRefReplacing (ExprAbstraction _ body lazy) =
  ExprAbstraction False (lambdaIncrementedArgRefsGreaterThanOrEqual (argRefPatchMin + 1) argRefReplacing body) lazy

lambdaIncrementedArgRefsGreaterThanOrEqual argRefPatchMin argRefReplacing (ExprApplication _ func arg') =
  ExprApplication False (incTerm func) (incTerm arg')
  where
    incTerm = lambdaIncrementedArgRefsGreaterThanOrEqual argRefPatchMin argRefReplacing

lambdaIncrementedArgRefsGreaterThanOrEqual _ _ ExprEmptyList =
  ExprEmptyList