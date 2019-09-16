{-# OPTIONS_GHC -Wall #-}

module Debrujin where

data Debrujin =
  DAR Int |
  DAB Debrujin |
  DAP Debrujin Debrujin
  deriving(Show, Eq)

unParseDebrujin :: Debrujin -> String
unParseDebrujin (DAR argRef) = show argRef
unParseDebrujin (DAB body) = "(/ " ++ unParseDebrujin body ++ ")"
unParseDebrujin (DAP func arg) = "(" ++ unParseDebrujin func ++ " " ++ unParseDebrujin arg ++ ")"