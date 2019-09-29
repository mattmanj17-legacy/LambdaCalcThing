{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE UndecidableInstances #-}

module Fallible where

import Control.Monad.Fail
import Control.Applicative

data FallibleT m a = FallibleT 
  { runFallible :: m (Either String a)
  }

instance (Show (m (Either String a))) => Show (FallibleT m a) where
  show = show . runFallible

instance Functor m => Functor (FallibleT m) where
  fmap f = FallibleT . fmap (fmap f) . runFallible

instance Applicative m => Applicative (FallibleT m) where
  pure = FallibleT . pure . Right
  f <*> x = FallibleT $ liftA2 (<*>) (runFallible f) (runFallible x)

instance Monad m => Monad (FallibleT m) where
  x >>= f = 
    FallibleT $ 
      runFallible x >>= 
        either 
          (return . Left) 
          (runFallible . f)

instance Monad m => MonadFail (FallibleT m) where
  fail = FallibleT . return . Left 