{-# OPTIONS_GHC -Wall #-}

module Lambda where

import Data.List

data Lambda =
  LAR String |
  LAB [String] Lambda |
  LAP [Lambda]
  deriving(Show, Eq)

unParseLambda :: Lambda -> String
unParseLambda (LAR str) = str
unParseLambda (LAB strs body) = "(/ [" ++ (intercalate " " strs) ++ "] " ++ unParseLambda body ++ ")"
unParseLambda (LAP terms) = "(" ++ (intercalate " " $ map unParseLambda terms) ++ ")"
