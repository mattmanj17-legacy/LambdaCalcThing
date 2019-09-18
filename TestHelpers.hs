module TestHelpers where

import Data.Maybe
import Test.Hspec 

runUnaryTestWithMaybeInput :: Show a => String -> (String -> a -> SpecWith ()) -> Maybe a -> SpecWith ()
runUnaryTestWithMaybeInput strDesc test ma = do
  it strDesc $ do
    ma `shouldSatisfy` isJust
  maybe (return ()) (test strDesc) ma


runBinaryTestWithMaybeInput :: Show a => String ->  (String -> a -> b -> SpecWith ()) -> Maybe a -> b -> SpecWith ()
runBinaryTestWithMaybeInput strDesc test ma b = do
  it strDesc $ do
    ma `shouldSatisfy` isJust
  maybe (return ()) (\a -> test strDesc a b) ma
