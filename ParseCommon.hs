{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}

module ParseCommon where

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim

import Data.Functor.Identity

parseWhiteSpace :: a -> GenParser Char st a
parseWhiteSpace a = do
  _ <- many (oneOf ['\x20','\x0D','\x0A','\x09'])
  return a

wrapWs :: GenParser Char st a -> GenParser Char st a
wrapWs parser = do
  parseWhiteSpace ()
  res <- parser
  parseWhiteSpace ()
  return res

parseFromStr :: Text.Parsec.Prim.Stream s Data.Functor.Identity.Identity t => Text.Parsec.Prim.Parsec s () a -> s -> Either ParseError a
parseFromStr parseFn = parse parseFn "unknown"