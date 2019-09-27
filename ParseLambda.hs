{-# OPTIONS_GHC -Wall #-}

module ParseLambda 
(
  parseLambda
)
where

import Text.ParserCombinators.Parsec

import ParseCommon

import LambdaAst

parseLambda :: SimpleParser LambdaParsed
parseLambda =
  parseId <|>
  parseList <|>
  parseParens

parseId :: SimpleParser LambdaParsed
parseId = do
  str <- wrapWs $ many1 letter
  return (LambdaParsedId str)

parseList :: SimpleParser LambdaParsed 
parseList = do
  _ <- wrapWs $ char '['
  elems <- sepBy (wrapWs parseLambda) (parseWhiteSpace ())
  _ <- wrapWs $ char ']'
  return (LambdaParsedList elems)

parseParens :: SimpleParser LambdaParsed
parseParens = do
  _ <- wrapWs $ char '('
  result <- parseApplication
  _ <- wrapWs $ char ')'
  return result

parseApplication :: SimpleParser LambdaParsed
parseApplication = do
  terms <- sepBy1 (wrapWs parseLambda) (parseWhiteSpace ())
  return (LambdaParsedApplication terms)