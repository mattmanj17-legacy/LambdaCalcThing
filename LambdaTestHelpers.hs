module LambdaTestHelpers where

import LambdaAst

cmpLambdaForTest :: LambdaAst -> LambdaAst -> Bool
cmpLambdaForTest = curry $ (boolFromTfn False) . (tfnCompareLambdas <$> fst <*> snd)
