{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}

module ParseCommon where

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim

import Data.Functor.Identity

type SimpleParser result = GenParser Char () result

parseWhiteSpace :: a -> SimpleParser a
parseWhiteSpace a = do
  _ <- many (oneOf ['\x20','\x0D','\x0A','\x09'])
  return a

wrapWs :: SimpleParser a -> SimpleParser a
wrapWs parser = do
  parseWhiteSpace ()
  res <- parser
  parseWhiteSpace ()
  return res

parseFromStr :: Stream s Identity t => Parsec s () a -> s -> Either ParseError a
parseFromStr parseFn = parse parseFn "unknown"