{-# OPTIONS_GHC -Wall #-}

module ParseDebrujin 
(
  parseDebrujin
)
where

import Text.ParserCombinators.Parsec

import Debrujin

import ParseCommon

parseDebrujin :: GenParser Char st Debrujin
parseDebrujin = do
  _ <- wrapWs $ char '('
  res <- (parseAbstraction <|> parseApplication)
  _ <- wrapWs $ char ')'
  return res

parseAbstraction :: GenParser Char st Debrujin
parseAbstraction = do
  _ <- wrapWs $ char '/'
  body <- wrapWs $ (parseDebrujin <|> parseArg)
  return (DAB body)

parseArg :: GenParser Char st Debrujin
parseArg = do
  argRef <- wrapWs $ many1 digit
  return (DAR (read argRef))

parseApplication :: GenParser Char st Debrujin
parseApplication = do
  func <- wrapWs $ (parseDebrujin <|> parseArg)
  arg <- wrapWs $ (parseDebrujin <|> parseArg)
  return (DAP func arg)