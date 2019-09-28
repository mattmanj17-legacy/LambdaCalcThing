{-# OPTIONS_GHC -Wall #-}

module Fallible where

import Control.Monad.Fail

newtype Fallible a = Fallible 
  { runFallible :: Either String a
  }

instance Functor Fallible where
  fmap fn (Fallible a) = Fallible $ fmap fn a

instance Applicative Fallible where
  pure a = Fallible $ pure a
  (Fallible a) <*> (Fallible b) = (Fallible (a <*> b))

instance Monad Fallible where
  (>>=) (Fallible a) fn = Fallible $ (>>=) a (runFallible . fn)
  (>>) (Fallible a) (Fallible b) = Fallible $ (>>) a b

instance MonadFail Fallible where
  fail str = Fallible (Left str)