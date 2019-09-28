{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module ReduceLambda where

import LambdaAst
import MetaData
import Fallible
import Data.Maybe

import Text.Parsec.Pos

errorStrAt :: AstMetaData -> String
errorStrAt (AstMetaData start end) = 
  concat
    [ "<"
    , (show (sourceLine start))
    , ":"
    , (show (sourceColumn start))
    , "-"
    , (show (sourceLine end))
    , ":"
    , (show (sourceColumn end))
    , ">"
    ]

-- ANON

anonLambda :: MetaData AstMetaData Ast -> Fallible Expr
anonLambda = replaceVars []

incReps :: [(String, Int)] -> [(String, Int)]
incReps = map ((,) <$> fst <*> (+1) . snd)

replaceVars :: [(String, Int)] -> MetaData AstMetaData Ast -> Fallible Expr
replaceVars reps (MetaData md repIn) =
  case repIn of
    (AstId str) -> replaceVarsInId reps md str
    (AstList elems) -> replaceVarsInList reps elems
    (AstApplication terms) -> replaceVarsInApp reps terms

replaceVarsInId :: [(String, Int)] -> AstMetaData -> String -> Fallible Expr
replaceVarsInId reps md str =
  maybe (fail $ (errorStrAt md) ++ " err1 could not find " ++ str) (return . ExprArgRef) (lookup str reps)

replaceVarsInList :: [(String, Int)] -> [MetaData AstMetaData Ast] -> Fallible Expr
replaceVarsInList reps elems = do
  newElems <- sequence $ map (replaceVars reps) elems
  return $ ExprList newElems

replaceVarsInApp :: [(String, Int)] -> [MetaData AstMetaData Ast] -> Fallible Expr
replaceVarsInApp 
  reps 
  [ (MetaData _ (AstId "fn"))
  , (MetaData md (AstId arg))
  , body
  ] =
  replaceVarsInAbsIdParam reps md arg body
replaceVarsInApp 
  reps 
  [ (MetaData fnMd (AstId "fn"))
  , (MetaData elemsMd (AstList elems))
  , body
  ] =
  case elems of
    ((MetaData mdArg (AstId arg)):rest) ->
      replaceVarsInAbsIdParam reps mdArg arg $
        case rest of
          [] -> 
            body
          _ ->
            (MetaData
              (AstMetaData (startPos fnMd) (endPos elemsMd))
              (AstApplication
                [(MetaData fnMd (AstId "fn"))
                , (MetaData (AstMetaData (endPos mdArg) (endPos elemsMd)) (AstList rest))
                , body
                ]
              )
            )
    ((MetaData md _):_) -> 
      fail $ (errorStrAt md) ++ " err4 non id in params list for fn"
    [] ->
      fail $ (errorStrAt elemsMd) ++ " err5 empty params list for fn"

replaceVarsInApp
  _
  ((MetaData fnMd (AstId "fn")):_) =
    fail $ (errorStrAt fnMd) ++ " err3 ill formed fn call"
replaceVarsInApp reps terms = do
  newTerms <- sequence $ map (replaceVars reps) terms
  return $ nestApps newTerms

replaceVarsInAbsIdParam :: [(String, Int)] -> AstMetaData -> String -> MetaData AstMetaData Ast -> Fallible Expr
replaceVarsInAbsIdParam reps strMd str body = do
  if isJust $ lookup str reps then
    fail $ (errorStrAt strMd) ++ " err2 replaceVarsInAbsIdParam blew up because we were going to shadow a param"
  else do
    let newreps = (str, 1):(incReps reps)
    newBody <- replaceVars newreps body
    return (ExprAbstraction newBody)

nestApps :: [Expr] -> Expr
nestApps [] = undefined
nestApps [_] = undefined
nestApps [a, b] = ExprApplication a b
nestApps (a:b:rest) = nestApps ((nestApps [a, b]):rest)

-- REDUX

lambdasBetaReducedOneStep :: [Expr] -> Fallible [Expr]
lambdasBetaReducedOneStep [] =
  return []

lambdasBetaReducedOneStep [term] = do
  reducedTerm <- lambdaBetaReducedOneStep term
  return [reducedTerm]

lambdasBetaReducedOneStep (term:rest) = do
  termReducedOnce <- lambdaBetaReducedOneStep term
  if term == termReducedOnce then do
    restReducedOnce <- lambdasBetaReducedOneStep rest
    return (term:restReducedOnce)
  else do
    return (termReducedOnce:rest)

lambdaBetaReducedOneStep :: Expr -> Fallible Expr
lambdaBetaReducedOneStep argRef@(ExprArgRef _) =
  return argRef

lambdaBetaReducedOneStep (ExprList elems) = do
  elemsReducedOnce <- lambdasBetaReducedOneStep elems
  return (ExprList elemsReducedOnce)

lambdaBetaReducedOneStep (ExprAbstraction val) = do
  reducedValOnce <- lambdaBetaReducedOneStep val
  return (ExprAbstraction reducedValOnce)

lambdaBetaReducedOneStep (ExprApplication (ExprAbstraction func) arg) =
  lambdaAppliedTo arg func

lambdaBetaReducedOneStep (ExprApplication func arg) = do
  [newFunc, newArg] <- lambdasBetaReducedOneStep [func, arg]
  return (ExprApplication newFunc newArg)


lambdaBetaReducedFull :: Expr -> Fallible Expr
lambdaBetaReducedFull term = do
  reducedOnce <- lambdaBetaReducedOneStep term
  if term == reducedOnce then do
    return term
  else do
    lambdaBetaReducedFull reducedOnce

lambdaAppliedTo :: Expr -> Expr -> Fallible Expr
lambdaAppliedTo = 
  lambdaArgRefReplacedWithLambda 1


lambdaArgRefReplacedWithLambda :: Int -> Expr -> Expr -> Fallible Expr
lambdaArgRefReplacedWithLambda argRefReplace arg (ExprArgRef argRef) = do
  if argRefReplace == argRef then do
    inced <- lambdaIncrementedArgRefsGreaterThanOrEqual arg 1 argRef
    return inced
  else if argRefReplace < argRef then
    return (ExprArgRef (argRef-1))
  else
    return (ExprArgRef argRef)

lambdaArgRefReplacedWithLambda argRefReplace arg (ExprList elems) = do
  elemsReplaced <- sequence (map (lambdaArgRefReplacedWithLambda argRefReplace arg) elems)
  return (ExprList elemsReplaced)

lambdaArgRefReplacedWithLambda argRefReplace arg (ExprAbstraction body) = do
  newBody <- lambdaArgRefReplacedWithLambda (argRefReplace+1) arg body
  return (ExprAbstraction newBody)

lambdaArgRefReplacedWithLambda argRefReplace argReplace (ExprApplication func arg) = do
  funcReplaced <- lambdaArgRefReplacedWithLambda argRefReplace argReplace func
  argReplaced <- lambdaArgRefReplacedWithLambda argRefReplace argReplace arg
  return (ExprApplication funcReplaced argReplaced)
  

lambdaIncrementedArgRefsGreaterThanOrEqual :: Expr -> Int -> Int -> Fallible Expr
lambdaIncrementedArgRefsGreaterThanOrEqual lar@(ExprArgRef argRef) argRefPatchMin argRefReplacing
  | argRef < argRefPatchMin = return lar
  | otherwise = return (ExprArgRef (argRef + argRefReplacing - 1))

lambdaIncrementedArgRefsGreaterThanOrEqual (ExprList elems) argRefPatchMin argRefReplacing = do
  let incElems lElem = lambdaIncrementedArgRefsGreaterThanOrEqual lElem argRefPatchMin argRefReplacing
  incedElems <- sequence (map incElems elems)
  return (ExprList incedElems)

lambdaIncrementedArgRefsGreaterThanOrEqual (ExprAbstraction body) argRefPatchMin argRefReplacing = do
  incedBody <- lambdaIncrementedArgRefsGreaterThanOrEqual body (argRefPatchMin + 1) argRefReplacing
  return (ExprAbstraction incedBody)

lambdaIncrementedArgRefsGreaterThanOrEqual (ExprApplication func arg) argRefPatchMin argRefReplacing = do
  let incTerm term = lambdaIncrementedArgRefsGreaterThanOrEqual term argRefPatchMin argRefReplacing
  incedFunc <- incTerm func
  incedArg <- incTerm arg
  return (ExprApplication incedFunc incedArg)