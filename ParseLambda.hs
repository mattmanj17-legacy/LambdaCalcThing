{-# OPTIONS_GHC -Wall #-}

module ParseLambda 
(
  parseLambda
)
where

import Text.ParserCombinators.Parsec

import ParseCommon

import LambdaAst

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

parseLambda :: SimpleParser Ast
parseLambda =
  parseId <|>
  parseList <|>
  parseParens

parseId :: SimpleParser Ast
parseId = do
  _ <- parseIgnore
  posStart <- getPosition
  str <- many1 letter
  posEnd <- getPosition
  _ <- parseIgnore
  return (AstId posStart posEnd str)

listToPairs :: SourcePos -> SourcePos -> [Ast] -> Ast
listToPairs sp ep [] = 
  AstEmptyList sp ep
listToPairs sp ep (a:rest) = 
    (AstPair sp ep a (listToPairs (endPos a) ep rest))

parseList :: SimpleParser Ast 
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
  return (listToPairs posStart posEnd elems)

parseParens :: SimpleParser Ast
parseParens = do
  _ <- parseIgnore
  _ <- char '('
  _ <- parseIgnore
  result <- parseApplication
  _ <- parseIgnore
  _ <- char ')'
  _ <- parseIgnore
  return result

listToApps :: SourcePos -> SourcePos -> [Ast] -> Ast
listToApps _ _ [] = undefined
listToApps _ _ [a] = a
listToApps sp ep (a:b:rest) = 
  listToApps sp ep 
    ((AstApplication (startPos a) (endPos b) a b):rest)

parseApplication :: SimpleParser Ast
parseApplication = do
  posStart <- getPosition
  terms <- sepBy1 parseLambda parseIgnore
  posEnd <- getPosition
  return (listToApps posStart posEnd terms)