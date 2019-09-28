{-# OPTIONS_GHC -Wall #-}

module MetaData where

data MetaData md d = MetaData
  { metaData :: md
  , rawData :: d
  }
  deriving(Show, Eq)

