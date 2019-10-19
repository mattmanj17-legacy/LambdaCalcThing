{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module LambdaAst where

import Control.Monad.Trans.Except
import Util
import Data.Ord

data LineColumn =
  LineColumn
    { getLineNumber :: Int
    , getColumn :: Int
    }
  deriving(Eq)

instance Show LineColumn where
  show lineColumn =
    line ++ ":" ++ column
    where
      line = show $ getLineNumber lineColumn
      column = show $ getColumn lineColumn

instance Ord LineColumn where
  compare =
    priCompareMany [comparing getLineNumber, comparing getColumn]

data SourceInfo =
  SourceInfo
    { getSourceName :: String
    , getStart :: LineColumn
    , getEnd :: LineColumn
    }
  deriving(Eq)

instance Show SourceInfo where
  show srcInf = 
    "<" ++ start ++ "-" ++ end ++ ">"
    where
      start = show $ getStart srcInf
      end = show $ getEnd srcInf

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

mkSrcInf :: (Monad m) => String -> LineColumn -> LineColumn -> ExceptT String m SourceInfo
mkSrcInf name start end =
  if end < start then
    throwE ("can not make a SourceInfo with start " ++ show start ++ " and end " ++ show end)
  else
    return $ SourceInfo name start end

mergeSrcInf :: (Monad m) => SourceInfo -> SourceInfo -> ExceptT String m SourceInfo
mergeSrcInf srcInfA srcInfB =
  if nameA == nameB then
    mkSrcInf nameA linColMin linColMax
  else
    throwE $ "can not merge source info from distinct sources " ++ nameA ++ " and " ++ nameB
  where
    nameA = getSourceName srcInfA
    nameB = getSourceName srcInfB
    linColMin = minimum $ map getStart [srcInfA, srcInfB]
    linColMax = maximum $ map getEnd [srcInfA, srcInfB]

mergeSrcInfFromAsts :: (Monad m) => AstR ->  AstR -> ExceptT String m SourceInfo
mergeSrcInfFromAsts astA astB =
  mergeSrcInf srcInfA srcInfB
  where
    srcInfA = getSrcInf astA
    srcInfB = getSrcInf astB

mkAstIdAt :: String -> SourceInfo -> AstR
mkAstIdAt idStr =
  AstR $ AstId $ AstIdR idStr

mkAstPair :: (Monad m) => AstR -> AstR -> ExceptT String m AstR
mkAstPair astFst astSnd = do
  srcInf <- mergeSrcInfFromAsts astFst astSnd
  return $ mkAstPairAt astFst astSnd srcInf

mkAstPairAt :: AstR -> AstR -> SourceInfo -> AstR
mkAstPairAt astFst astSnd =
  AstR $ AstPair $ AstPairR astFst astSnd

mkAstEmptyListAt :: SourceInfo -> AstR
mkAstEmptyListAt = 
  AstR $ AstEmptyList

mkAstApp :: (Monad m) => AstR -> AstR -> ExceptT String m AstR
mkAstApp astFn astArg = do
  srcInf <- mergeSrcInfFromAsts astFn astArg
  return $ mkAstAppAt astFn astArg srcInf

mkAstAppAt :: AstR -> AstR -> SourceInfo -> AstR
mkAstAppAt astFn astArg = 
  AstR $ AstApplication $ AstApplicationR astFn astArg

getStartLine :: SourceInfo -> Int
getStartLine = getLineNumber . getStart

getEndLine :: SourceInfo -> Int
getEndLine = getLineNumber . getEnd

getStartColumn :: SourceInfo -> Int
getStartColumn = getColumn . getStart

getEndColumn :: SourceInfo -> Int
getEndColumn = getColumn . getEnd

getAstStartLine :: AstR -> Int
getAstStartLine = getStartLine . getSrcInf

getAstStartColumn :: AstR -> Int
getAstStartColumn = getStartColumn . getSrcInf

getAstEndLine :: AstR -> Int
getAstEndLine = getEndLine . getSrcInf

getAstEndColumn :: AstR -> Int
getAstEndColumn = getEndColumn . getSrcInf

pairifyAstList :: (Monad m) => SourceInfo -> [AstR] -> ExceptT String m AstR
pairifyAstList srcInf asts = do
  folded <- foldrM mkAstPair (mkAstEmptyListAt srcInf) asts
  case getAst folded of
    AstPair pairAst ->
      return $ mkAstPairAt foldedFst foldedSnd srcInf
      where
        foldedFst = getFstAst pairAst
        foldedSnd = getSndAst pairAst
    _ ->
      return folded

appifyAstList :: (Monad m) => SourceInfo -> AstR -> [AstR] -> ExceptT String m AstR
appifyAstList srcInf astHead astRest = do
  folded <- foldlM mkAstApp astHead astRest
  case astRest of -- have to tell if we ever actully ran mkAstApp. If not, return astHead unaltered
    [] ->
      return astHead
    _ ->
      case getAst folded of
        AstApplication astApp ->
          return $ mkAstAppAt foldedFn foldedArg srcInf
          where
            foldedFn = getFnAst astApp
            foldedArg = getArgAst astApp
        _ ->
          return folded