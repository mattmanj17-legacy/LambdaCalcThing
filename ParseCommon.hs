{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}

module ParseCommon where

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim

import Data.Functor.Identity

import Control.Monad.Trans.Except

type SimpleParser result = GenParser Char () result

parseWhiteSpace :: a -> SimpleParser a
parseWhiteSpace a = do
  _ <- many (oneOf ['\x20','\x0D','\x0A','\x09'])
  return a

parseWhiteSpace1 :: a -> SimpleParser a
parseWhiteSpace1 a = do
  _ <- many1 (oneOf ['\x20','\x0D','\x0A','\x09'])
  return a

parseFallible :: 
  (Stream s Identity t, Monad m) => 
  Parsec s () a -> 
  SourceName -> 
  s -> 
  ExceptT String m a
parseFallible parseFn file = (either (throwE . show) return) . (parse parseFn file)