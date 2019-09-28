{-# OPTIONS_GHC -Wall #-}

module ParseLambda 
(
  parseLambda
)
where

import Text.ParserCombinators.Parsec

import ParseCommon

import LambdaAst
import MetaData

parseIgnore :: SimpleParser ()
parseIgnore = do
  _ <- many $ parseWhiteSpace1 () <|> parseComment
  return ()

parseComment :: SimpleParser ()
parseComment = do
  _ <- char '-'
  _ <- char '-'
  _ <- many (noneOf "\n")
  return ()

parseLambda :: SimpleParser (MetaData AstMetaData Ast)
parseLambda =
  parseId <|>
  parseList <|>
  parseParens

parseId :: SimpleParser (MetaData AstMetaData Ast)
parseId = do
  _ <- parseIgnore
  posStart <- getPosition
  str <- many1 letter
  posEnd <- getPosition
  _ <- parseIgnore
  return (MetaData (AstMetaData posStart posEnd) (AstId str))

parseList :: SimpleParser (MetaData AstMetaData Ast) 
parseList = do
  _ <- parseIgnore
  posStart <- getPosition
  _ <- char '['
  _ <- parseIgnore
  elems <- sepBy parseLambda parseIgnore
  _ <- parseIgnore
  _ <- char ']'
  posEnd <- getPosition
  _ <- parseIgnore
  return (MetaData (AstMetaData posStart posEnd) (AstList elems))

parseParens :: SimpleParser (MetaData AstMetaData Ast)
parseParens = do
  _ <- parseIgnore
  posStart <- getPosition
  _ <- char '('
  _ <- parseIgnore
  result <- parseApplication
  _ <- parseIgnore
  _ <- char ')'
  posEnd <- getPosition
  _ <- parseIgnore
  return (MetaData (AstMetaData posStart posEnd) result)

parseApplication :: SimpleParser Ast
parseApplication = do
  terms <- sepBy1 parseLambda parseIgnore
  return (AstApplication terms)