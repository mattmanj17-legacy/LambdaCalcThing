{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Util where

import Control.Monad.Trans.Except

insert :: Int -> [a] -> [a] -> [a]
insert n xsInsert xsModify =
  xsStart ++ xsInsert ++ xsEnd
  where
    (xsStart, xsEnd) = splitAt n xsModify

mapFirst :: (a -> a) -> [a] -> [a] 
mapFirst _ [] = []
mapFirst fn (x:xs) = (fn x):xs

mapLast :: (a -> a) -> [a] -> [a]
mapLast _ [] = []
mapLast fn [x] = [fn x]
mapLast fn (x:xs) = x:(mapLast fn xs)

foldrM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldrM _ d [] = 
  return d
foldrM f d (x:xs) = do
  z <- foldrM f d xs
  f x z

foldlM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldlM _ d [] =
  return d
foldlM f d (x:xs) = do
  z <- f d x
  foldlM f z xs

priCompare :: (a -> a -> Ordering) -> (a -> a -> Ordering) -> a -> a -> Ordering
priCompare cmpA cmpB =
  curry $ mappend <$> uncurry cmpA <*> uncurry cmpB

priCompareMany :: [a -> a -> Ordering] -> a -> a -> Ordering
priCompareMany = foldl1 priCompare

flattenExceptT :: Functor m => ExceptT s (ExceptT s m) a -> ExceptT s m a
flattenExceptT e =
  ExceptT $ fmap (either Left id) $ runExceptT $ runExceptT e
