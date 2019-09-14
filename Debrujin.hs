{-# OPTIONS_GHC -Wall #-}

module Debrujin where

data Debrujin =
  DAR Int |
  DAB Debrujin |
  DAP Debrujin Debrujin
  deriving(Eq)

instance Show Debrujin where
  show (DAR argRef) = show argRef
  show (DAB body) = "(/ " ++ show body ++ ")"
  show (DAP func arg) = "(" ++ show func ++ " " ++ show arg ++ ")"