{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Util where

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