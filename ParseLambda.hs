{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

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
parseLambda = do
  _ <- parseIgnore
  parsed <- parseId <|> parseList <|> parseApplication
  _ <- parseIgnore
  return parsed

parseId :: SimpleParser Ast
parseId = do
  posStart <- getPosition
  str <- many1 letter
  posEnd <- getPosition
  return (AstId posStart posEnd str)

listToPairs :: SourcePos -> SourcePos -> [Ast] -> Ast
listToPairs sp ep [] = 
  AstEmptyList sp ep
listToPairs sp ep (a:rest) = 
  (AstPair sp ep a (listToPairs (srcposAstEnd a) ep rest))

parseList :: SimpleParser Ast 
parseList = do
  posStart <- getPosition
  _ <- char '['
  _ <- parseIgnore
  elems <- sepBy parseLambda parseIgnore
  _ <- parseIgnore
  _ <- char ']'
  posEnd <- getPosition
  return (listToPairs posStart posEnd elems)

parseApplication :: SimpleParser Ast
parseApplication = do
  posStart <- getPosition
  _ <- char '('
  _ <- parseIgnore
  terms <- sepBy1 parseLambda parseIgnore
  _ <- parseIgnore
  _ <- char ')'
  posEnd <- getPosition
  return (listToApps posStart posEnd terms)

listToApps :: SourcePos -> SourcePos -> [Ast] -> Ast
listToApps _ _ [] = undefined
listToApps _ _ [a] = a
listToApps sp ep [a, b] = 
  (AstApplication sp ep a b)
listToApps sp ep (a:b:rest) = 
  listToApps sp ep 
    ((AstApplication (srcposAstStart a) (srcposAstEnd b) a b):rest)