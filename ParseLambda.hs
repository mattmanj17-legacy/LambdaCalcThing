{-# OPTIONS_GHC -Wall #-}

module ParseLambda 
(
  parseLambda
)
where

import Text.ParserCombinators.Parsec

import ParseCommon

import LambdaAst

parseLambda :: SimpleParser LambdaAst
parseLambda =
  parseId <|>
  parseArgRef <|>
  parseList <|>
  parseParens

parseId :: SimpleParser LambdaAst
parseId = do
  str <- wrapWs $ many1 letter
  return (LambdaId str)

parseArgRef :: SimpleParser LambdaAst
parseArgRef = do
  _ <- wrapWs $ char '#'
  str <- wrapWs $ many1 digit
  return (LambdaArgRef (read str))

parseList :: SimpleParser LambdaAst 
parseList = do
  _ <- wrapWs $ char '['
  elems <- sepBy (wrapWs parseLambda) (parseWhiteSpace ())
  _ <- wrapWs $ char ']'
  return (LambdaList elems)

parseParens :: SimpleParser LambdaAst
parseParens = do
  _ <- wrapWs $ char '('
  result <- (parseAbstraction <|> parseAnonAbstraction <|> parseApplication)
  _ <- wrapWs $ char ')'
  return result

parseAbstraction :: SimpleParser LambdaAst
parseAbstraction = do
  _ <- wrapWs $ char '/'
  params <- wrapWs parseLambda
  body <- wrapWs parseLambda
  return (LambdaAbstraction params body)

parseAnonAbstraction :: SimpleParser LambdaAst
parseAnonAbstraction = do
  _ <- wrapWs $ char '%'
  body <- wrapWs parseLambda
  return (LambdaAnonAbstraction body)

parseApplication :: SimpleParser LambdaAst
parseApplication = do
  terms <- sepBy1 (wrapWs parseLambda) (parseWhiteSpace ())
  return (LambdaApplication terms)