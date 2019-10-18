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

data AstId = 
  AstId
    { getIdStr :: String
    }
  deriving(Eq)

data AstPair = 
  AstPair 
    { getFstAst :: Ast
    , getSndAst :: Ast
    }
  deriving(Eq)

data AstApplication = 
  AstApplication 
    { getFnAst :: Ast
    , getArgAst :: Ast
    }
  deriving(Eq)

data AstNode =
  IdNode AstId |
  PairNode AstPair |
  EmptyListNode |
  ApplicationNode AstApplication
  deriving(Eq)

data Ast =
  Ast
    { getAstNode :: AstNode
    , getSrcInf :: SourceInfo
    }
  deriving(Eq)

instance Show Ast where
  show ast =
    case getAstNode ast of
      IdNode astId -> 
        getIdStr astId
      EmptyListNode -> 
        "[]"
      PairNode astPair -> 
        concat ["<", show fstAst, ", ", show sndAst, ">"]
        where
          fstAst = getFstAst astPair
          sndAst = getSndAst astPair
      ApplicationNode astApp ->
        concat ["(", show fnAst, " ", show argAst, ")"]
        where
          fnAst = getFnAst astApp
          argAst = getArgAst astApp

mkAstIdAt :: String -> SourceInfo -> Ast
mkAstIdAt idStr =
  Ast $ IdNode $ AstId idStr

mkAstPair :: Ast -> Ast -> Ast
mkAstPair astFst astSnd =
  mkAstPairAt astFst astSnd srcInf
  where
    startPos = getAstStartPos astFst
    endPos = getAstEndPos astSnd
    srcInf = SourceInfo startPos endPos

mkAstPairAt :: Ast -> Ast -> SourceInfo -> Ast
mkAstPairAt astFst astSnd =
  Ast $ PairNode $ AstPair astFst astSnd

mkAstEmptyListAt :: SourceInfo -> Ast
mkAstEmptyListAt = 
  Ast $ EmptyListNode

mkAstApp :: Ast -> Ast -> Ast
mkAstApp astFn astArg =
  mkAstAppAt astFn astArg srcInf
  where
    startPos = getAstStartPos astFn
    endPos = getAstEndPos astArg
    srcInf = SourceInfo startPos endPos

mkAstAppAt :: Ast -> Ast -> SourceInfo -> Ast
mkAstAppAt astFn astArg = 
  Ast $ ApplicationNode $ AstApplication astFn astArg

getAstEndPos :: Ast -> SourcePos
getAstEndPos = getEndPos . getSrcInf

getAstStartPos :: Ast -> SourcePos
getAstStartPos = getStartPos . getSrcInf