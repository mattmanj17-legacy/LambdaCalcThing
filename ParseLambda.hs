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
  let astId = mkAstIdAt str $ SourceInfo posStart posEnd
  return astId

listToPairs :: SourceInfo -> [Ast] -> Ast
listToPairs srcInf list =
  mkAstPairAt foldedFst foldedSnd srcInf
  where
    (PairNode folded) = getAstNode $ foldr mkAstPair (mkAstEmptyListAt srcInf) list
    foldedFst = getFstAst folded
    foldedSnd = getSndAst folded

parseList :: SimpleParser Ast 
parseList = do
  posStart <- getPosition
  _ <- char '['
  _ <- parseIgnore
  elems <- sepBy parseLambda parseIgnore
  _ <- parseIgnore
  _ <- char ']'
  posEnd <- getPosition
  let srcInf = SourceInfo posStart posEnd
  return (listToPairs srcInf elems)

parseApplication :: SimpleParser Ast
parseApplication = do
  posStart <- getPosition
  _ <- char '('
  _ <- parseIgnore
  terms <- sepBy1 parseLambda parseIgnore
  _ <- parseIgnore
  _ <- char ')'
  posEnd <- getPosition
  let srcInf = SourceInfo posStart posEnd
  return (listToApps srcInf terms)

listToApps :: SourceInfo -> [Ast] -> Ast
listToApps srcInf list =
  mkAstAppAt foldedFn foldedArg srcInf
  where
    (ApplicationNode folded) = getAstNode $ foldl1 mkAstApp list
    foldedFn = getFnAst folded
    foldedArg = getArgAst folded