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
    , crunchApp :: ([result] -> result)
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
  args <- sepBy1 (wrapWs ((parseLambdaLike config) <|> (parseArg config))) (parseWhiteSpace ())
  return (crunchApp config (func:args))

parseLambdaParams :: SimpleParser [String]
parseLambdaParams = do
  _ <- wrapWs $ char '['
  strs <- (sepBy1 (many1 letter) (parseWhiteSpace ()))
  _ <- wrapWs $ char ']'
  return strs

parseLambda :: SimpleParser Lambda
parseLambda = 
  parseLambdaLike 
    (LambdaLikeParseConfig 
      { parseArgFn=(many1 letter) 
      , crunchArg=LAR 
      , crunchApp=LAP 
      , parsePreAbs=parseLambdaParams
      , crunchAbs=LAB 
      }
    )

crunchDebrujinApp :: [Debrujin] -> Debrujin
crunchDebrujinApp [] = 
  error "crunchDebrujinApp blew up on empty list"
crunchDebrujinApp [_] = 
  error "crunchDebrujinApp blew up single elem list"
crunchDebrujinApp [func, arg] = 
  (DAP func arg)
crunchDebrujinApp (func:arg:rest) =
  crunchDebrujinApp ((DAP func arg):rest)

parseDebrujin :: SimpleParser Debrujin
parseDebrujin = 
  parseLambdaLike 
    (LambdaLikeParseConfig 
      { parseArgFn=(many1 digit) 
      , crunchArg=(DAR . read) 
      , crunchApp=crunchDebrujinApp 
      , parsePreAbs=(return ()) 
      , crunchAbs=(\_ body -> DAB body)
      }
    )