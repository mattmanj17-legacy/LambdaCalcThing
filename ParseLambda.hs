{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module ParseLambda 
(
  parseLambda
)
where

import Text.ParserCombinators.Parsec
import Control.Monad.Trans.Except

import ParseCommon

import LambdaAst

import Control.Monad.Trans.Class

parseIgnore :: Monad m => SimpleParserT m ()
parseIgnore = do
  _ <- many $ parseWhiteSpace1 () <|> parseComment
  return ()

parseComment :: Monad m => SimpleParserT m ()
parseComment = do
  _ <- char '-'
  _ <- char '-'
  _ <- many (noneOf "\n")
  return ()

parseLambda :: Monad m => SimpleParserT (ExceptT String m) AstR
parseLambda = do
  _ <- parseIgnore
  parsed <- parseId <|> parseList <|> parseApplication
  _ <- parseIgnore
  return parsed

parseId :: Monad m => SimpleParserT (ExceptT String m) AstR
parseId = do
  posStart <- getPosition
  str <- many1 letter
  posEnd <- getPosition
  srcInf <- lift $ mkSrcInfFromPoss posStart posEnd
  return $ mkAstIdAt str srcInf

parseList :: Monad m => SimpleParserT (ExceptT String m) AstR 
parseList = do
  posStart <- getPosition
  _ <- char '['
  _ <- parseIgnore
  elems <- sepBy parseLambda parseIgnore
  _ <- parseIgnore
  _ <- char ']'
  posEnd <- getPosition
  srcInf <- lift $ mkSrcInfFromPoss posStart posEnd
  lift $ pairifyAstList srcInf elems

parseApplication :: Monad m => SimpleParserT (ExceptT String m) AstR
parseApplication = do
  posStart <- getPosition
  _ <- char '('
  _ <- parseIgnore
  headTerm <- parseLambda
  _ <- parseIgnore
  restTerms <- sepBy1 parseLambda parseIgnore
  _ <- parseIgnore
  _ <- char ')'
  posEnd <- getPosition
  srcInf <- lift $ mkSrcInfFromPoss posStart posEnd
  lift $ appifyAstList srcInf headTerm restTerms

mkSrcInfFromPoss :: Monad m => SourcePos -> SourcePos -> ExceptT String m SourceInfo
mkSrcInfFromPoss posStart posEnd = do
  srcInfStart <- mkSrcInfFromPos posStart
  srcInfEnd <- mkSrcInfFromPos posEnd
  mergeSrcInf srcInfStart srcInfEnd

mkSrcInfFromPos :: Monad m => SourcePos -> ExceptT String m SourceInfo
mkSrcInfFromPos pos =
  mkSrcInf (sourceName pos) lineCol lineCol
  where
    lineCol = mkLineColFromPos pos

mkLineColFromPos :: SourcePos -> LineColumn
mkLineColFromPos =
  LineColumn <$> sourceLine <*> sourceColumn