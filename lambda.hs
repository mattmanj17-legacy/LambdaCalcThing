{-# OPTIONS_GHC -Wall #-}

module Lambda where

data Lambda =
  LAR String |
  LAB String Lambda |
  LAP Lambda Lambda
  deriving(Show, Eq)

unParseLambda :: Lambda -> String
unParseLambda (LAR str) = str
unParseLambda (LAB str body) = "(/ " ++ str ++ " " ++ unParseLambda body ++ ")"
unParseLambda (LAP func arg) = "(" ++ unParseLambda func ++ " " ++ unParseLambda arg ++ ")"
