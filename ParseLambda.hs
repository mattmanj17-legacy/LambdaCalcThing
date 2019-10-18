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
  let astId = AstId posStart posEnd str False
  return astId

listToPairs :: SourcePos -> SourcePos -> [Ast] -> Ast
listToPairs start end xss = 
  case xss of
    [] -> 
      AstEmptyList start end False
    x:xs -> 
      AstPair start end x xsToPairs False
      where
        xEnd = getEnd x
        xsToPairs = listToPairs xEnd end xs

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
listToApps start end list =
  AstApplication start end foldedFn foldedArg False
  where
    folded = foldl1 mkAstApp list
    foldedFn = getFnAst folded
    foldedArg = getArgAst folded