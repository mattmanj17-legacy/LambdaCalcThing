{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE UndecidableInstances #-}

module Fallible where

import Control.Applicative
import Data.Functor.Identity 

data FallibleT m a = FallibleT 
  { runFallible :: m (Either String a)
  }

fallibleLiftM :: Functor m => m a -> FallibleT m a
fallibleLiftM x = FallibleT (fmap Right x)

fallibleLiftId :: Monad m => FallibleT Identity a -> FallibleT m a
fallibleLiftId x = FallibleT (return (runIdentity (runFallible x)))

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

throwE :: Monad m => String -> FallibleT m a
throwE = FallibleT . return . Left