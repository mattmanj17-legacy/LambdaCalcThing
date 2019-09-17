module TestHelpers where

import Data.Maybe
import Test.Hspec 

runUnaryTestWithMaybeInput :: Show a => (a -> SpecWith ()) -> Maybe a -> SpecWith ()
runUnaryTestWithMaybeInput test ma = do
  it ("check input") $ do
    ma `shouldSatisfy` isJust
  maybe (return ()) (test) ma


runBinaryTestWithMaybeInput :: Show a => (a -> b -> SpecWith ()) -> Maybe a -> b -> SpecWith ()
runBinaryTestWithMaybeInput test ma b = do
  it ("check input") $ do
    ma `shouldSatisfy` isJust
  maybe (return ()) (\a -> test a b) ma
