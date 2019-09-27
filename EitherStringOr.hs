{-# OPTIONS_GHC -Wall #-}

module EitherStringOr where


import Prelude hiding (fail)
import Control.Monad.Fail

newtype EitherStringOr a = EitherStringOr (Either String a)

instance Show a => Show (EitherStringOr a) where
  show (EitherStringOr (Left str)) = str
  show (EitherStringOr (Right a)) = show a

eitherFromEitherStringOr :: EitherStringOr a -> Either String a
eitherFromEitherStringOr (EitherStringOr a) = a

isEsRight :: EitherStringOr a -> Bool
isEsRight = not . isEsLeft

fromEsRightUnsafe :: EitherStringOr a -> a
fromEsRightUnsafe (EitherStringOr (Right a)) = a
fromEsRightUnsafe _ = undefined

isEsLeft :: EitherStringOr a -> Bool
isEsLeft (EitherStringOr (Left _)) = True
isEsLeft _ = False

instance Functor EitherStringOr where
  fmap fn (EitherStringOr a) = EitherStringOr $ fmap fn a

instance Applicative EitherStringOr where
  pure a = EitherStringOr $ Right a
  (<*>) (EitherStringOr (Left str)) _ = (EitherStringOr (Left str))
  (<*>) (EitherStringOr (Right fn)) a = fmap fn a

instance Monad EitherStringOr where
  (>>=) (EitherStringOr a) fn = EitherStringOr $ (>>=) a (eitherFromEitherStringOr . fn)
  (>>) (EitherStringOr a) (EitherStringOr b) = EitherStringOr $ (>>) a b

instance MonadFail EitherStringOr where
  fail str = EitherStringOr (Left str)