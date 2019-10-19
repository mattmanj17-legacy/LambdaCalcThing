{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE FlexibleContexts #-} -- Needed for the type of parseExceptT

module ParseCommon where

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim

import Control.Monad.Trans.Except

import Control.Monad.Trans.Class

type SimpleParserT m result = ParsecT String () m result

parseWhiteSpace :: Monad m => a -> SimpleParserT m a
parseWhiteSpace a = do
  _ <- many (oneOf ['\x20','\x0D','\x0A','\x09'])
  return a

parseWhiteSpace1 :: Monad m => a -> SimpleParserT m a
parseWhiteSpace1 a = do
  _ <- many1 (oneOf ['\x20','\x0D','\x0A','\x09'])
  return a

parseExceptT :: 
  Monad m => 
  SimpleParserT m a ->
  SourceName -> 
  String -> 
  ExceptT String m a
parseExceptT parseFn fileName fileStr = do
  parsed <- lift $ runPT parseFn () fileName fileStr
  either (throwE . show) return parsed