
module ParseLambdaLikeTests
(
  parseLambdaLikeTests
) 
where

import Data.Either

import Text.ParserCombinators.Parsec
import Test.Hspec

import ParseCommon
import ParseLambdaLike
import Lambda
import Debrujin

parseTests :: 
  (Show a, Eq a) => 
  String -> 
  (String -> Either ParseError a) -> 
  (String -> a -> SpecWith (), String -> SpecWith ())
parseTests strType parseFn =
  (pass, fail)
  where
    pass strInput expect =
      it ("parse pass " ++ strType ++ " " ++ strInput) $ do 
        (parseFn strInput) `shouldBe` (Right expect)
    fail strInput =
      it ("parse fail " ++ strType ++ " " ++ strInput) $ do 
        (parseFn strInput) `shouldNotSatisfy` isRight

parseLambdaLikeToValueTest ::
  (Show result, Eq result) =>
  String -> 
  LambdaLikeParseConfig a b result ->  
  (String -> result -> SpecWith (), String -> SpecWith ())
parseLambdaLikeToValueTest strType config =
  parseTests strType $ parseFromStr $ parseLambdaLike $ config

parseLambdaLikeToNullTest :: 
  String -> 
  SimpleParser a -> 
  SimpleParser b -> 
  (String -> SpecWith (), String -> SpecWith ())
parseLambdaLikeToNullTest strType parseArg parsePre =
  (\str -> pass str (), fail)
  where
    (pass, fail) =
      parseLambdaLikeToValueTest 
        strType
        LambdaLikeParseConfig 
          { parseArgFn=(parseArg) 
          , crunchArg=(const ()) 
          , crunchApp=(\_ _ -> ()) 
          , parsePreAbs=(parsePre) 
          , crunchAbs=(\_ _ -> ()) 
          }

(nullPass, nullFail) = parseLambdaLikeToNullTest "null" (return ()) (return ())
(abPass, abFail) = parseLambdaLikeToNullTest "AB" (char 'a') (char 'b')
(aPass, aFail) = parseLambdaLikeToNullTest "A" (char 'a') (return ())
(bPass, bFail) = parseLambdaLikeToNullTest "B" (return ()) (char 'b')

(nullToAB, _) =
  parseLambdaLikeToValueTest 
    "NullToAB"
    (LambdaLikeParseConfig 
      { parseArgFn=(return "a") 
      , crunchArg=(id) 
      , crunchApp=(\func arg -> "(" ++ func ++ " " ++ arg ++ ")")
      , parsePreAbs=(return "b") 
      , crunchAbs=(\preBody body -> "(/ " ++ preBody ++ " " ++ body ++ ")")
      }
    )

(nullToInt, _) =
  parseLambdaLikeToValueTest 
    "NullToInt"
    (LambdaLikeParseConfig 
      { parseArgFn=(return 1) 
      , crunchArg=(id) 
      , crunchApp=(+)
      , parsePreAbs=(return 2) 
      , crunchAbs=(+)
      }
    )

(passLambda, failLambda) = 
  parseTests "lambda" (parseFromStr parseLambda)

(passDebrujin, failDebrujin) = 
  parseTests "debrujin" (parseFromStr parseDebrujin)

parseLambdaLikeTests = do
  nullPass "()"
  nullPass "(/)"
  nullPass "(())"
  nullPass "(() ())"
  nullPass "(/ ())"
  nullPass "(/ (/))"
  nullPass "((((()())))())"
  nullPass "((/((/(/))))(/))"

  nullFail ""
  nullFail "(() () ())"
  nullFail "(/ /)"
  nullFail "(()"
  nullFail "(/()"
  nullFail "((/)"
  nullFail "a"
  nullFail "(a a)"
  nullFail "(/ b a)"
  nullFail "((a a) a)"
  nullFail "(a (a a))"
  nullFail "((a a) (a a))"
  nullFail "(/ b (a a))"
  nullFail "(/ b (/ b a))"
  nullFail "(((((a a)(a a)) a) a)(a a))"
  nullFail "((/ b ((/ b (/ b a)) a))(/ b a))"

  abPass "(a a)"
  abPass "(/ b a)"
  abPass "((a a) a)"
  abPass "(a (a a))"
  abPass "((a a) (a a))"
  abPass "(/ b (a a))"
  abPass "(/ b (/ b a))"
  abPass "(((((a a)(a a)) a) a)(a a))"
  abPass "((/ b ((/ b (/ b a)) a))(/ b a))"

  abFail ""
  abFail "a"
  abFail "()"
  abFail "(/)"
  abFail "(())"
  abFail "(() ())"
  abFail "(/ ())"
  abFail "(/ (/))"
  abFail "((((()())))())"
  abFail "((/((/(/))))(/))"

  aPass "(a a)"
  aPass "(/ a)"
  aPass "((a a) a)"
  aPass "(a (a a))"
  aPass "((a a) (a a))"
  aPass "(/ (a a))"
  aPass "(/ (/ a))"
  aPass "(((((a a)(a a)) a) a)(a a))"
  aPass "((/ ((/ (/ a)) a))(/ a))"

  aFail ""
  aFail "a"
  aFail "()"
  aFail "(/)"
  aFail "(())"
  aFail "(() ())"
  aFail "(/ ())"
  aFail "(/ (/))"
  aFail "((((()())))())"
  aFail "((/((/(/))))(/))"

  bPass "()"
  bPass "(/ b)"
  bPass "(())"
  bPass "(()())"
  bPass "(/ b ())"
  bPass "(/ b (/ b))"
  bPass "((((()())))())"
  bPass "((/ b ((/ b (/ b))))(/ b))"

  bFail ""
  bFail "a"
  bFail "(/)"
  bFail "(/ ())"
  bFail "(/ (/))"
  bFail "((/((/(/))))(/))"

  nullToAB "()" "(a a)"
  nullToAB "(/)" "(/ b a)"
  nullToAB "(())" "((a a) a)"
  nullToAB "(() ())" "((a a) (a a))"
  nullToAB "(/ ())" "(/ b (a a))"
  nullToAB "(/ (/))" "(/ b (/ b a))"
  nullToAB "((((()())))())" "(((((a a) (a a)) a) a) (a a))"
  nullToAB "((/((/(/))))(/))" "((/ b ((/ b (/ b a)) a)) (/ b a))"

  nullToInt "()" 2
  nullToInt "(/)" 3
  nullToInt "(())" 3
  nullToInt "(() ())" 4
  nullToInt "(/ ())" 4
  nullToInt "(/ (/))" 5
  nullToInt "((((()())))())" 8
  nullToInt "((/((/(/))))(/))" 11

  passLambda "(a a)" (LAP (LAR "a") (LAR "a"))
  passLambda "(foo bar)" (LAP (LAR "foo") (LAR "bar"))
  passLambda "(/ b a)" (LAB "b" (LAR "a"))
  passLambda "(/ foo bar)" (LAB "foo" (LAR "bar"))
  passLambda "(/ a a)" (LAB "a" (LAR "a"))
  passLambda "((a a) a)" (LAP (LAP (LAR "a") (LAR "a")) (LAR "a"))
  passLambda "(a (a a))" (LAP (LAR "a") (LAP (LAR "a") (LAR "a")))
  passLambda "((a a) (a a))" (LAP (LAP (LAR "a") (LAR "a")) (LAP (LAR "a") (LAR "a")))
  passLambda "(/ b (a a))"(LAB "b" (LAP (LAR "a") (LAR "a")))
  passLambda "(/ b (/ b a))" (LAB "b" (LAB "b" (LAR "a")))
  passLambda "(((((a a)(a a)) a) a)(a a))" (LAP (LAP (LAP (LAP (LAP (LAR "a") (LAR "a")) (LAP (LAR "a") (LAR "a"))) (LAR "a")) (LAR "a")) (LAP (LAR "a") (LAR "a")))
  passLambda "((/ b ((/ b (/ b a)) a))(/ b a))" (LAP (LAB "b" (LAP (LAB "b" (LAB "b" (LAR "a"))) (LAR "a"))) (LAB "b" (LAR "a")))

  failLambda "(1 a)"
  failLambda "(/ 1 a)" 
  failLambda "(/ b 1)"
  failLambda ""
  failLambda "a"
  failLambda "()"
  failLambda "(/)"
  failLambda "(())"
  failLambda "(() ())"
  failLambda "(/ ())"
  failLambda "(/ (/))"
  failLambda "((((()())))())"
  failLambda "((/((/(/))))(/))"
  failLambda "(/ a)"
  failLambda "(/ (a a))"
  failLambda "(/ (/ a))"
  failLambda "((/ ((/ (/ a)) a))(/ a))"
  failLambda "(/ b)"
  failLambda "(/ b ())"
  failLambda "(/ b (/ b))"
  failLambda "((/ b ((/ b (/ b))))(/ b))"

  passDebrujin "(1 2)" (DAP (DAR 1) (DAR 2))
  passDebrujin "(/ 1)" (DAB (DAR 1))
  passDebrujin "((2 1) 3)" (DAP (DAP (DAR 2) (DAR 1)) (DAR 3))
  passDebrujin "(4 (6 6))" (DAP (DAR 4) (DAP (DAR 6) (DAR 6)))
  passDebrujin "((1 2) (2 1))" (DAP (DAP (DAR 1) (DAR 2)) (DAP (DAR 2) (DAR 1)))
  passDebrujin "(/ (2 2))" (DAB (DAP (DAR 2) (DAR 2)))
  passDebrujin "(/ (/ 1))" (DAB (DAB (DAR 1)))
  passDebrujin "(((((1 1)(1 1)) 1) 1)(1 1))" (DAP (DAP (DAP (DAP (DAP (DAR 1) (DAR 1)) (DAP (DAR 1) (DAR 1))) (DAR 1)) (DAR 1)) (DAP (DAR 1) (DAR 1)))
  passDebrujin "((/ ((/ (/ 1)) 1))(/ 1))" (DAP (DAB (DAP (DAB (DAB (DAR 1))) (DAR 1))) (DAB (DAR 1)))

  failDebrujin ""
  failDebrujin "a"
  failDebrujin "()"
  failDebrujin "(/)"
  failDebrujin "(())"
  failDebrujin "(() ())"
  failDebrujin "(/ ())"
  failDebrujin "(/ (/))"
  failDebrujin "((((()())))())"
  failDebrujin "((/((/(/))))(/))"
  failDebrujin "(/ b)"
  failDebrujin "(/ b ())"
  failDebrujin "(/ b (/ b))"
  failDebrujin "((/ b ((/ b (/ b))))(/ b))"
  failDebrujin "(a a)"
  failDebrujin "(/ b a)"
  failDebrujin "((a a) a)"
  failDebrujin "(a (a a))"
  failDebrujin "((a a) (a a))"
  failDebrujin "(/ b (a a))"
  failDebrujin "(/ b (/ b a))"
  failDebrujin "(((((a a)(a a)) a) a)(a a))"
  failDebrujin "((/ b ((/ b (/ b a)) a))(/ b a))"
  failDebrujin "(/ 1 1)"
  failDebrujin "(/ 1 (2 2))"
  failDebrujin "(/ 1 (/ 1 1))"
  failDebrujin "((/ 1 ((/ 1 (/ 1 1)) 1))(/ 1 1))"