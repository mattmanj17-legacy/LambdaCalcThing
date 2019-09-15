{-# OPTIONS_GHC -Wall #-}

module ParseLambdaLike 
(
  parseLambda,
  parseDebrujin,
  LambdaLikeParseConfig(..),
  parseLambdaLike
)
where

import Text.ParserCombinators.Parsec

import ParseCommon

import Lambda
import Debrujin

data LambdaLikeParseConfig a b result = 
  LambdaLikeParseConfig 
    { parseArgFn :: (SimpleParser a)
    , crunchArg :: (a -> result)
    , crunchApp :: (result -> result -> result)
    , parsePreAbs :: (SimpleParser b)
    , crunchAbs :: (b -> result -> result)
    }

parseLambdaLike :: LambdaLikeParseConfig a b result -> SimpleParser result
parseLambdaLike config = do
  _ <- wrapWs $ char '('
  res <- ((parseAbstraction config) <|> (parseApplication config))
  _ <- wrapWs $ char ')'
  return res

parseAbstraction :: LambdaLikeParseConfig a b result -> SimpleParser result
parseAbstraction config = do
  _ <- wrapWs $ char '/'
  preBody <- wrapWs (parsePreAbs config)
  body <- wrapWs $ ((parseLambdaLike config) <|> (parseArg config))
  return (crunchAbs config preBody body)

parseArg :: LambdaLikeParseConfig a b result -> SimpleParser result
parseArg config = do
  arg <- wrapWs (parseArgFn config)
  return (crunchArg config arg)

parseApplication :: LambdaLikeParseConfig a b result -> SimpleParser result
parseApplication config = do
  func <- wrapWs ((parseLambdaLike config) <|> (parseArg config))
  arg <- wrapWs ((parseLambdaLike config) <|> (parseArg config))
  return (crunchApp config func arg)

parseLambda :: SimpleParser Lambda
parseLambda = 
  parseLambdaLike 
    (LambdaLikeParseConfig 
      { parseArgFn=(many1 letter) 
      , crunchArg=LAR 
      , crunchApp=LAP 
      , parsePreAbs=(many1 letter) 
      , crunchAbs=LAB 
      }
    )

parseDebrujin :: SimpleParser Debrujin
parseDebrujin = 
  parseLambdaLike 
    (LambdaLikeParseConfig 
      { parseArgFn=(many1 digit) 
      , crunchArg=(DAR . read) 
      , crunchApp=DAP 
      , parsePreAbs=(return ()) 
      , crunchAbs=(\_ body -> DAB body)
      }
    )