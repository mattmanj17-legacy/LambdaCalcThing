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

parseLambda :: SimpleParser AstR
parseLambda = do
  _ <- parseIgnore
  parsed <- parseId <|> parseList <|> parseApplication
  _ <- parseIgnore
  return parsed

parseId :: SimpleParser AstR
parseId = do
  posStart <- getPosition
  str <- many1 letter
  posEnd <- getPosition
  let astId = mkAstIdAt str $ SourceInfo posStart posEnd
  return astId

parseList :: SimpleParser AstR 
parseList = do
  posStart <- getPosition
  _ <- char '['
  _ <- parseIgnore
  elems <- sepBy parseLambda parseIgnore
  _ <- parseIgnore
  _ <- char ']'
  posEnd <- getPosition
  let srcInf = SourceInfo posStart posEnd
  return (pairifyAstList srcInf elems)

parseApplication :: SimpleParser AstR
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
  let srcInf = SourceInfo posStart posEnd
  return (appifyAstList srcInf headTerm restTerms)