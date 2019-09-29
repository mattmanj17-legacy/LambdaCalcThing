{-# OPTIONS_GHC -Wall #-}

module ParseLambda 
(
  parseLambda
)
where

import Text.ParserCombinators.Parsec

import ParseCommon

import LambdaAst
import MetaData

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

parseLambda :: SimpleParser (MetaData AstMetaData Ast)
parseLambda =
  parseId <|>
  parseList <|>
  parseParens

parseId :: SimpleParser (MetaData AstMetaData Ast)
parseId = do
  _ <- parseIgnore
  posStart <- getPosition
  str <- many1 letter
  posEnd <- getPosition
  _ <- parseIgnore
  return (MetaData (AstMetaData posStart posEnd) (AstId str))

listToPairs :: MetaData AstMetaData [MetaData AstMetaData Ast] -> MetaData AstMetaData Ast
listToPairs (MetaData md []) = 
  MetaData md AstEmptyList
listToPairs (MetaData md (a:rest)) = 
  (MetaData md 
    (AstPair 
      a 
      (listToPairs 
        (MetaData 
          (AstMetaData (endPos (metaData a)) (endPos md))
          rest
        )
      )
    )
  )

parseList :: SimpleParser (MetaData AstMetaData Ast) 
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
  return (listToPairs (MetaData (AstMetaData posStart posEnd) elems))

parseParens :: SimpleParser (MetaData AstMetaData Ast)
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
  return (MetaData (AstMetaData posStart posEnd) (rawData result))

listToApps :: MetaData AstMetaData [MetaData AstMetaData Ast] -> MetaData AstMetaData Ast
listToApps (MetaData _ []) = undefined
listToApps (MetaData _ [a]) = a
listToApps (MetaData md (a:b:rest)) = 
  listToApps 
    (MetaData md 
      ((:)
        (MetaData
          (AstMetaData (startPos (metaData a)) (endPos (metaData b)))
          (AstApplication a b)
        )
        rest
      )
    )

parseApplication :: SimpleParser (MetaData AstMetaData Ast)
parseApplication = do
  posStart <- getPosition
  terms <- sepBy1 parseLambda parseIgnore
  posEnd <- getPosition
  return (listToApps (MetaData (AstMetaData posStart posEnd) terms))