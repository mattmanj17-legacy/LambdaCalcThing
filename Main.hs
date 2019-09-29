{-# OPTIONS_GHC -Wall #-}

import Test.Hspec

import LambdaAst
import Fallible
import ParseLambda
import MetaData
--import UnAnonLambda
import ReduceLambda
import ParseCommon
import Data.Functor.Identity
import Data.Either

main :: IO ()
main = do
  s <- readFile "test.txt"
  doIt s

doIt :: String -> IO ()
doIt str =
  hspec $ do
    let parsed = parseFallible parseLambda "test.txt" str :: FallibleT Identity (MetaData AstMetaData Ast)
    describe "lambda" $ do
      it "should parse" $ do
        parsed `shouldSatisfy` (isRight . runIdentity . runFallible)
      if isRight (runIdentity (runFallible parsed)) then do
        let justParsed = fromRight undefined (runIdentity (runFallible parsed))
        let compiled = runIdentity $ runFallible $ anonLambda justParsed
        it "should compile" $ do
          compiled `shouldSatisfy` isRight
        if isRight compiled then do
          let justCompiled = fromRight undefined compiled
          let reduced = runIdentity $ runFallible $ lambdaBetaReducedFull justCompiled
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

genTests :: String -> FallibleT Identity [FallibleT Identity (Expr, Expr)]
genTests str = do
  parsed <- parseFallible parseLambda "test.txt" str
  compiled <- anonLambda parsed
  reduced <- lambdaBetaReducedFull compiled
  case reduced of
    (ExprList elems) -> return $ (map verifyTest) elems
    _ -> throwE "expr was not a pair!!!"

verifyTest :: Expr -> FallibleT Identity (Expr, Expr)
verifyTest (ExprList [a, b]) = return (a, b)
verifyTest _ = throwE "expr was not a pair!!!"

singleTest :: Expr -> SpecWith ()
singleTest (ExprList [a, b]) = do
  it "should equal" $ do
    a `shouldBe` b
singleTest _ =
  it "was not a list1" $ do
    True `shouldBe` False