{-# OPTIONS_GHC -Wall #-}

import Data.Maybe
import Test.Hspec
import Text.ParserCombinators.Parsec

import ParseCommon
import Lambda
import Debrujin
import ReduceDebrujin
import ParseLambdaLike
import LambdaToDebrujin

fromRightUnsafe :: Either a b -> b
fromRightUnsafe (Right b) = b
fromRightUnsafe (Left _) = error "fromRightUnsafe blew up"

parseLambdaUnsafe :: String -> Lambda
parseLambdaUnsafe = fromRightUnsafe . (parseFromStr parseLambda)

parseLambdaTest :: String -> Lambda -> SpecWith ()
parseLambdaTest str expect =
   it ("parseLambdaTest " ++ str) $ do
    (parseLambdaUnsafe str) `shouldBe` expect

parseDebrujinUnsafe :: String -> Debrujin
parseDebrujinUnsafe = fromRightUnsafe . (parseFromStr parseDebrujin)

anonStr :: String -> Debrujin
anonStr = fromJust . lambdaBoundVarsAnonymized . parseLambdaUnsafe

anonStrTest :: String -> Debrujin -> SpecWith ()
anonStrTest str expect =
   it ("anonStrTest " ++ str) $ do
    (anonStr str) `shouldBe` expect

reduceStrOnce :: String -> Debrujin
reduceStrOnce = lambdaBetaReducedOneStep . anonStr

reduceStrOnceTest :: String -> Debrujin -> SpecWith ()
reduceStrOnceTest str expect =
   it ("reduceStrOnceTest " ++ str) $ do
    (reduceStrOnce str) `shouldBe` expect

reduceOnceTest :: Debrujin -> Debrujin -> SpecWith ()
reduceOnceTest start expect =
   it ("reduceOnceTest " ++ show start) $ do
    (lambdaBetaReducedOneStep start) `shouldBe` expect

churchNum :: Int -> Debrujin
churchNum n = (DAB (DAB (churchNumHelper n)))
  where
    churchNumHelper 0 = (DAR 1)
    churchNumHelper m = (DAP (DAR 2) (churchNumHelper (m-1)))

binIntOpTest :: String -> Debrujin -> (Int -> Int -> Int) -> Int -> Int -> SpecWith ()
binIntOpTest strType expr op a b =
  it ("binIntOpTest " ++ strType ++ " " ++ show a ++ " " ++ show b) $ do 
    (lambdaBetaReducedFull (DAP (DAP expr (churchNum a)) (churchNum b))) `shouldBe` (churchNum (op a b))

addOperator :: Debrujin
addOperator = (DAB (DAB (DAB (DAB (DAP (DAP (DAR 4) (DAR 2)) (DAP (DAP (DAR 3) (DAR 2)) (DAR 1)))))))

addExprTest :: Int -> Int -> SpecWith ()
addExprTest = binIntOpTest "add" addOperator (+)

multOperator :: Debrujin
multOperator = (anonStr "(/ m (/ n (/ g (m (n g)))))")

multExprTest :: Int -> Int -> SpecWith ()
multExprTest = binIntOpTest "mul" multOperator (*)

parseLambdaLikeTest :: String -> SimpleParser a -> SimpleParser b -> String -> SpecWith ()
parseLambdaLikeTest strType parseArg parsePre strInput =
  it ("parseLambdaLikeTest " ++ strType ++ " " ++ strInput) $ do 
    (parseLambdaLikeCustom strInput) `shouldBe` (Right ())
  where
    parseLambdaLikeCustom =
      parseFromStr $
        parseLambdaLike $
          LambdaLikeParseConfig 
            { parseArgFn=(parseArg) 
            , crunchArg=(const ()) 
            , crunchApp=(\_ _ -> ()) 
            , parsePreAbs=(parsePre) 
            , crunchAbs=(\_ _ -> ()) 
            }
        
parseNullTest :: String -> SpecWith ()
parseNullTest = parseLambdaLikeTest "null" (return ()) (return ())
        
parseABTest :: String -> SpecWith ()
parseABTest = parseLambdaLikeTest "null" (char 'a') (char 'b')
        
parseATest :: String -> SpecWith ()
parseATest = parseLambdaLikeTest "null" (char 'a') (return ())
        
parseBTest :: String -> SpecWith ()
parseBTest = parseLambdaLikeTest "null" (return ()) (char 'b')

parseNullToAB :: SimpleParser String
parseNullToAB = 
  parseLambdaLike 
    (LambdaLikeParseConfig 
      { parseArgFn=(return "a") 
      , crunchArg=(id) 
      , crunchApp=(\func arg -> "(" ++ func ++ " " ++ arg ++ ")")
      , parsePreAbs=(return "b") 
      , crunchAbs=(\preBody body -> "(/ " ++ preBody ++ " " ++ body ++ ")")
      }
    )

parseNullToABTest :: String -> String -> SpecWith ()
parseNullToABTest strIn strOut = 
    it ("parseNullToABTest " ++ strIn ++ " " ++ strOut) $ do 
        (parseFromStr parseNullToAB strIn) `shouldBe` (Right strOut)

parseNullToInt :: SimpleParser Int
parseNullToInt = 
  parseLambdaLike 
    (LambdaLikeParseConfig 
      { parseArgFn=(return 1) 
      , crunchArg=(id) 
      , crunchApp=(+)
      , parsePreAbs=(return 2) 
      , crunchAbs=(+)
      }
    )

parseNullToIntTest :: String -> Int -> SpecWith ()
parseNullToIntTest strIn nOut = 
    it ("parseNullToIntTest " ++ strIn ++ " " ++ show nOut) $ do 
        (parseFromStr parseNullToInt strIn) `shouldBe` (Right nOut)

main :: IO ()
main = hspec $ do
  describe "lambda" $ do
    parseNullTest "()"
    parseNullTest "(/)"
    parseNullTest "(())"
    parseNullTest "(() ())"
    parseNullTest "(/ ())"
    parseNullTest "(/ (/))"
    parseNullTest "((((()())))())"
    parseNullTest "((/((/(/))))(/))"

    parseABTest "(a a)"
    parseABTest "(/ b a)"
    parseABTest "((a a) a)"
    parseABTest "(a (a a))"
    parseABTest "((a a) (a a))"
    parseABTest "(/ b (a a))"
    parseABTest "(/ b (/ b a))"
    parseABTest "(((((a a)(a a)) a) a)(a a))"
    parseABTest "((/ b ((/ b (/ b a)) a))(/ b a))"

    parseATest "(a a)"
    parseATest "(/ a)"
    parseATest "((a a) a)"
    parseATest "(a (a a))"
    parseATest "((a a) (a a))"
    parseATest "(/ (a a))"
    parseATest "(/ (/ a))"
    parseATest "(((((a a)(a a)) a) a)(a a))"
    parseATest "((/ ((/ (/ a)) a))(/ a))"

    parseBTest "()"
    parseBTest "(/ b)"
    parseBTest "(())"
    parseBTest "(()())"
    parseBTest "(/ b ())"
    parseBTest "(/ b (/ b))"
    parseBTest "((((()())))())"
    parseBTest "((/ b ((/ b (/ b))))(/ b))"

    parseNullToABTest "()" "(a a)"
    parseNullToABTest "(/)" "(/ b a)"
    parseNullToABTest "(())" "((a a) a)"
    parseNullToABTest "(() ())" "((a a) (a a))"
    parseNullToABTest "(/ ())" "(/ b (a a))"
    parseNullToABTest "(/ (/))" "(/ b (/ b a))"
    parseNullToABTest "((((()())))())" "(((((a a) (a a)) a) a) (a a))"
    parseNullToABTest "((/((/(/))))(/))" "((/ b ((/ b (/ b a)) a)) (/ b a))"

    parseNullToIntTest "()" 2
    parseNullToIntTest "(/)" 3
    parseNullToIntTest "(())" 3
    parseNullToIntTest "(() ())" 4
    parseNullToIntTest "(/ ())" 4
    parseNullToIntTest "(/ (/))" 5
    parseNullToIntTest "((((()())))())" 8
    parseNullToIntTest "((/((/(/))))(/))" 11

    parseLambdaTest "(/ x x)" (LAB "x" (LAR "x"))
    parseLambdaTest "(/ x y)" (LAB "x" (LAR "y"))
    parseLambdaTest "(/ y (/ x (x y)))" (LAB "y" (LAB "x" (LAP (LAR "x") (LAR "y"))))
    parseLambdaTest "((/ x x)(/ x x))" (LAP (LAB "x" (LAR "x")) (LAB "x" (LAR "x")))
   
    anonStrTest "(/ x x)" (DAB (DAR 1))
    anonStrTest "(/ y (/ x (x y)))" (DAB (DAB (DAP (DAR 1) (DAR 2))))
    anonStrTest "(/ y (/ x x))" (DAB (DAB (DAR 1)))
    anonStrTest "((/ x x)(/ x x))" (DAP (DAB (DAR 1)) (DAB (DAR 1)))
    anonStrTest "((/ x (/ y x))(/ x x))" (DAP (DAB (DAB (DAR 2))) (DAB (DAR 1)))
    anonStrTest "(/ m (/ n (/ f (/ x ((m f) ((n f) x))))))" addOperator
    anonStrTest "(/ f (/ x x))" (churchNum 0)
    anonStrTest "(/ f (/ x (f x)))" (churchNum 1)
    anonStrTest "(/ f (/ x (f (f x))))" (churchNum 2)
    anonStrTest "(/ f (/ x (f (f (f x)))))" (churchNum 3)
    
    reduceStrOnceTest "((/ x x)(/ x x))" (anonStr "(/ x x)")
    reduceStrOnceTest "((/ x (/ y x))(/ x x))" (anonStr "(/ y (/ x x))")
    reduceStrOnceTest "((/ x (/ y (/ z (/ w x)))) (/ a (/ b (/ c (/ d a)))))" (anonStr "(/ y (/ z (/ w (/ a (/ b (/ c (/ d a)))))))")

    addExprTest 0 0
    addExprTest 0 1
    addExprTest 1 0
    addExprTest 1 1
    addExprTest 1 2
    addExprTest 2 1
    addExprTest 3 2
    addExprTest 2 3

    multExprTest 0 0
    multExprTest 0 1
    multExprTest 1 0
    
    multExprTest 1 1
    multExprTest 1 2
    multExprTest 2 1
    multExprTest 3 2
    multExprTest 2 3