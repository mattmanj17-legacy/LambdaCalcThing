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

parseLambda :: SimpleParser RichParsedLambda
parseLambda =
  parseId <|>
  parseList <|>
  parseParens

parseId :: SimpleParser RichParsedLambda
parseId = do
  _ <- parseIgnore
  posStart <- getPosition
  str <- many1 letter
  posEnd <- getPosition
  _ <- parseIgnore
  return (RichParsedLambda posStart (LambdaParsedId str) posEnd)

parseList :: SimpleParser RichParsedLambda 
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
  return (RichParsedLambda posStart (LambdaParsedList elems) posEnd)

parseParens :: SimpleParser RichParsedLambda
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
  return (RichParsedLambda posStart result posEnd)

parseApplication :: SimpleParser LambdaParsed
parseApplication = do
  terms <- sepBy1 parseLambda parseIgnore
  return (LambdaParsedApplication terms)