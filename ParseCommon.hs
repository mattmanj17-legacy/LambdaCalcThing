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

parseWhiteSpace1 :: a -> SimpleParser a
parseWhiteSpace1 a = do
  _ <- many1 (oneOf ['\x20','\x0D','\x0A','\x09'])
  return a

parseFromStr :: Stream s Identity t => Parsec s () a -> s -> Either ParseError a
parseFromStr parseFn = parse parseFn "unknown"

parseFromStrToEither :: Stream s Identity t => Parsec s () a -> s -> Either String a
parseFromStrToEither parseFn = (either (Left . show) Right) . (parseFromStr parseFn)