{-# OPTIONS_GHC -Wall #-}

module ParseLambda 
(
  parseLambda
)
where

import Text.ParserCombinators.Parsec

import Lambda

import ParseCommon

parseLambda :: GenParser Char st Lambda
parseLambda = do
  _ <- wrapWs $ char '('
  res <- (parseAbstraction <|> parseApplication)
  _ <- wrapWs $ char ')'
  return res

parseAbstraction :: GenParser Char st Lambda
parseAbstraction = do
  _ <- wrapWs $ char '/'
  arg <- wrapWs $ many1 letter
  body <- wrapWs $ (parseLambda <|> parseLambdaArg)
  return (LAB arg body)

parseLambdaArg :: GenParser Char st Lambda
parseLambdaArg = do
  name <- wrapWs $ many1 letter
  return (LAR name)

parseApplication :: GenParser Char st Lambda
parseApplication = do
  func <- wrapWs $ (parseLambda <|> parseLambdaArg)
  arg <- wrapWs $ (parseLambda <|> parseLambdaArg)
  return (LAP func arg)