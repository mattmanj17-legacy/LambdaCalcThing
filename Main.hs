{-# OPTIONS_GHC -Wall #-}

import Test.Hspec

import LambdaAst
import EitherStringOr
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
        let compiled = anonLambda justParsed
        it "should compile" $ do
          compiled `shouldSatisfy` isEsRight
        if isEsRight compiled then do
          let justCompiled = fromEsRightUnsafe compiled
          let reduced = lambdaBetaReducedFull justCompiled
          it "should reduce" $ do
            reduced `shouldSatisfy` isEsRight
          if isEsRight reduced then do
            let justReduced = fromEsRightUnsafe reduced
            case justReduced of
              (LambdaCompiledList tests) -> do
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

singleTest :: LambdaCompiled -> SpecWith ()
singleTest (LambdaCompiledList [a, b]) = do
  it "should equal" $ do
    a `shouldBe` b
singleTest _ =
  it "was not a list1" $ do
    True `shouldBe` False