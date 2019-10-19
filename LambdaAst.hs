{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module LambdaAst where

import Text.Parsec.Pos

data SourceInfo =
  SourceInfo
    { getStartPos :: SourcePos
    , getEndPos :: SourcePos
    }
  deriving(Eq)

data AstIdR = 
  AstIdR
    { getIdStr :: String
    }
  deriving(Eq)

instance Show AstIdR where
  show = getIdStr

data AstPairR = 
  AstPairR 
    { getFstAst :: AstR
    , getSndAst :: AstR
    }
  deriving(Eq)

instance Show AstPairR where
  show astPair =
    "<" ++ show fstAst ++ ", " ++ show sndAst ++ ">"
    where
      fstAst = getFstAst astPair
      sndAst = getSndAst astPair

data AstApplicationR = 
  AstApplicationR 
    { getFnAst :: AstR
    , getArgAst :: AstR
    }
  deriving(Eq)

instance Show AstApplicationR where
  show astApp =
    "(" ++ show fnAst ++ " " ++ show argAst ++ ")"
    where
      fnAst = getFnAst astApp
      argAst = getArgAst astApp

data Ast =
  AstId AstIdR |
  AstPair AstPairR |
  AstEmptyList |
  AstApplication AstApplicationR
  deriving(Eq)

instance Show Ast where
  show astNode =
    case astNode of
      AstId astId -> show astId
      AstEmptyList -> "[]"
      AstPair astPair -> show astPair
      AstApplication astApp -> show astApp

data AstR =
  AstR
    { getAst :: Ast
    , getSrcInf :: SourceInfo
    }
  deriving(Eq)

instance Show AstR where
  show = show . getAst

mkAstIdAt :: String -> SourceInfo -> AstR
mkAstIdAt idStr =
  AstR $ AstId $ AstIdR idStr

mkAstPair :: AstR -> AstR -> AstR
mkAstPair astFst astSnd =
  mkAstPairAt astFst astSnd srcInf
  where
    startPos = getAstStartPos astFst
    endPos = getAstEndPos astSnd
    srcInf = SourceInfo startPos endPos

mkAstPairAt :: AstR -> AstR -> SourceInfo -> AstR
mkAstPairAt astFst astSnd =
  AstR $ AstPair $ AstPairR astFst astSnd

mkAstEmptyListAt :: SourceInfo -> AstR
mkAstEmptyListAt = 
  AstR $ AstEmptyList

mkAstApp :: AstR -> AstR -> AstR
mkAstApp astFn astArg =
  mkAstAppAt astFn astArg srcInf
  where
    startPos = getAstStartPos astFn
    endPos = getAstEndPos astArg
    srcInf = SourceInfo startPos endPos

mkAstAppAt :: AstR -> AstR -> SourceInfo -> AstR
mkAstAppAt astFn astArg = 
  AstR $ AstApplication $ AstApplicationR astFn astArg

getAstEndPos :: AstR -> SourcePos
getAstEndPos = getEndPos . getSrcInf

getAstStartPos :: AstR -> SourcePos
getAstStartPos = getStartPos . getSrcInf

pairifyAstList :: SourceInfo -> [AstR] -> AstR
pairifyAstList srcInf asts =
  case getAst folded of
    AstPair pairAst ->
      mkAstPairAt foldedFst foldedSnd srcInf
      where
        foldedFst = getFstAst pairAst
        foldedSnd = getSndAst pairAst
    _ ->
      folded
  where
    folded = foldr mkAstPair (mkAstEmptyListAt srcInf) asts

appifyAstList :: SourceInfo -> AstR -> [AstR] -> AstR
appifyAstList srcInf astHead astRest =
  case astRest of
    [] ->
      astHead
    _ ->
      case getAst folded of
        AstApplication astApp ->
          mkAstAppAt foldedFn foldedArg srcInf
          where
            foldedFn = getFnAst astApp
            foldedArg = getArgAst astApp
        _ ->
          folded
      where
        folded = foldl mkAstApp astHead astRest
    