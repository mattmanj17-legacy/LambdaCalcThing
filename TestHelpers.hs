module TestHelpers where

import Data.Either
import Test.Hspec 

runUnaryTestWithMaybeInput :: Show a => String -> (String -> a -> SpecWith ()) -> Either String a -> SpecWith ()
runUnaryTestWithMaybeInput strDesc test ma = do
  it strDesc $ do
    ma `shouldSatisfy` isRight
  either (const $ return ()) (test strDesc) ma


runBinaryTestWithMaybeInput :: Show a => String ->  (String -> a -> b -> SpecWith ()) -> Either String a -> b -> SpecWith ()
runBinaryTestWithMaybeInput strDesc test ma b = do
  it strDesc $ do
    ma `shouldSatisfy` isRight
  either (const $ return ()) (\a -> test strDesc a b) ma
