{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module LambdaAst where

import Text.Parsec.Pos

-- Lambda ast, the "sweet" version we parse in

data Ast =
  AstId 
    { getStart :: SourcePos
    , getEnd :: SourcePos
    , getIdStr :: String
    , foo :: Bool
    } |
  AstEmptyList 
    { getStart :: SourcePos
    , getEnd :: SourcePos
    , foo :: Bool 
    } |
  AstPair 
    { getStart :: SourcePos
    , getEnd :: SourcePos
    , getFstAst :: Ast
    , getSndAst :: Ast
    , foo :: Bool 
    } |
  AstApplication 
    { getStart :: SourcePos
    , getEnd :: SourcePos
    , getFnAst :: Ast
    , getArgAst :: Ast
    , foo :: Bool 
    }
  deriving(Eq)

mkAstApp :: Ast -> Ast -> Ast
mkAstApp fn arg = 
  AstApplication start end fn arg False
  where
    start = getStart fn
    end = getEnd arg

isAstId :: Ast -> Bool
isAstId AstId {} = True
isAstId _ = False

isAstApp :: Ast -> Bool
isAstApp AstApplication {} = True
isAstApp _ = False

instance Show Ast where
  show ast =
    case ast of
      AstId {} -> idStr
      AstEmptyList {} -> "[]"
      AstPair {} -> concat ["<", show fstAst, ", ", show sndAst, ">"]
      AstApplication {} -> concat ["(", show fnAst, " ", show argAst, ")"]
    where
      idStr = getIdStr ast
      fstAst = getFstAst ast
      sndAst = getSndAst ast
      fnAst = getFnAst ast
      argAst = getArgAst ast