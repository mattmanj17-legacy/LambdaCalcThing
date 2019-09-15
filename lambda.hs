{-# OPTIONS_GHC -Wall #-}

module Lambda where

data Lambda =
  LAR String |
  LAB String Lambda |
  LAP Lambda Lambda
  deriving(Eq)

instance Show Lambda where
  show (LAR str) = str
  show (LAB str body) = "(/ " ++ str ++ " " ++ show body ++ ")"
  show (LAP func arg) = "(" ++ show func ++ " " ++ show arg ++ ")"
