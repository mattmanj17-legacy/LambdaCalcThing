{-# OPTIONS_GHC -Wall #-}

import Test.Hspec

import LambdaAst
import Fallible
import ParseLambda
--import UnAnonLambda
import ReduceLambda
import Data.Either
import ParseCommon

main :: IO ()
main = do
  s <- readFile "test.txt"
  doIt s

doIt :: String -> IO ()
doIt str =
  hspec $ do
    let parsed = parseFromStrToEither parseLambda str
    describe "lambda" $ do
      it "should parse" $ do
        parsed `shouldSatisfy` isRight
      if isRight parsed then do
        let justParsed = fromRight undefined parsed
        let compiled = runFallible $ anonLambda justParsed
        it "should compile" $ do
          compiled `shouldSatisfy` isRight
        if isRight compiled then do
          let justCompiled = fromRight undefined compiled
          let reduced = runFallible $ lambdaBetaReducedFull justCompiled
          it "should reduce" $ do
            reduced `shouldSatisfy` isRight
          if isRight reduced then do
            let justReduced = fromRight undefined reduced
            case justReduced of
              (ExprList tests) -> do
                _ <- sequence $ map singleTest tests
                it "honk" $ do
                  True `shouldBe` True
              _ ->
                it "was not a list1" $ do
                  True `shouldBe` False
          else do
            it "honk" $ do
              True `shouldBe` True
        else do
          it "honk" $ do
            True `shouldBe` True
      else do
        it "honk" $ do
          True `shouldBe` True

singleTest :: Expr -> SpecWith ()
singleTest (ExprList [a, b]) = do
  it "should equal" $ do
    a `shouldBe` b
singleTest _ =
  it "was not a list1" $ do
    True `shouldBe` False