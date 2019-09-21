
module ParseLambdaTests
(
  parseLambdaTests
) 
where

import Data.Either
import Data.List

import Text.ParserCombinators.Parsec
import Test.Hspec

import ParseCommon
import ParseLambda
import LambdaAst

parseTests :: 
  (Show a) => 
  (a -> a -> Bool) ->
  (String -> Either ParseError a) -> 
  (String -> String -> a -> SpecWith (), String -> String -> SpecWith ())
parseTests eqFn parseFn =
  (pass, fail)
  where
    pass strDesc strInput expect =
      it strDesc $ do 
        let parsed = parseFn strInput
        parsed `shouldSatisfy` isRight
        if isLeft parsed then do
          return ()
        else
          eqFn (fromRight undefined parsed) expect `shouldBe` True
    fail strDesc strInput =
      it strDesc $ do 
        (parseFn strInput) `shouldNotSatisfy` isRight

(passLambda, failLambda) = 
  parseTests 
    (curry ((boolFromTfn False) . (tfnCompareLambdas <$> fst <*> snd)))
    (parseFromStr parseLambda)

parseLambdaTests = do
  passLambda "pl0"
    "(a a)" 
    (LambdaApplication [(LambdaId "a"), (LambdaId "a")])
  passLambda "pl1"
    "(a a a)"
    (LambdaApplication [(LambdaId "a"), (LambdaId "a"), (LambdaId "a")])
  passLambda "pl2"
    "(a (a a) a)"
    (LambdaApplication 
      [
        (LambdaId "a"), 
        (LambdaApplication [(LambdaId "a"), (LambdaId "a")]), 
        (LambdaId "a")
      ]
    )
  passLambda "pl3"
    "((a a) a a)"
    (LambdaApplication 
      [
        (LambdaApplication [(LambdaId "a"), (LambdaId "a")]), 
        (LambdaId "a"), 
        (LambdaId "a")
      ]
    )
  passLambda "pl4"
    "(a a (a a))"
    (LambdaApplication 
      [
        (LambdaId "a"), 
        (LambdaId "a"), 
        (LambdaApplication [(LambdaId "a"), (LambdaId "a")])
      ]
    )
  passLambda "pl5"
    "(/ [a b c] a)"
    (LambdaAbstraction 
      (LambdaList [(LambdaId "a"), (LambdaId "b"), (LambdaId "c")]) 
      (LambdaId "a")
    )
  passLambda "pl6"
    "(/ [a b c] (a b))"
    (LambdaAbstraction 
      (LambdaList [(LambdaId "a"), (LambdaId "b"), (LambdaId "c")])
      (LambdaApplication 
        [
          (LambdaId "a"), 
          (LambdaId "b")
        ]
      )
    )
  passLambda "pl7"
    "(/ [a b c] (a b c))"
    (LambdaAbstraction 
      (LambdaList [(LambdaId "a"), (LambdaId "b"), (LambdaId "c")])
      (LambdaApplication 
        [
          (LambdaId "a"), 
          (LambdaId "b"), 
          (LambdaId "c")
        ]
      )
    )
  passLambda "pl8"
    "(/ [a b c] (a (b c)))"
    (LambdaAbstraction 
      (LambdaList [(LambdaId "a"), (LambdaId "b"), (LambdaId "c")])
      (LambdaApplication 
        [
          (LambdaId "a"), 
          (LambdaApplication 
            [
              (LambdaId "b"), 
              (LambdaId "c")
            ]
          )
        ]
      )
    )
  passLambda "pl9"
    "(foo bar)"
    (LambdaApplication 
      [
        (LambdaId "foo"), 
        (LambdaId "bar")
      ]
    )
  passLambda "pl10"
    "(/ b a)"
    (LambdaAbstraction 
      (LambdaId "b") 
      (LambdaId "a")
    )
  passLambda "pl11"
    "(/ foo bar)"
    (LambdaAbstraction 
      (LambdaId "foo") 
      (LambdaId "bar")
    )
  passLambda "pl12"
    "(/ a a)"
    (LambdaAbstraction 
      (LambdaId "a")  
      (LambdaId "a")
    )
  passLambda "pl13"
    "((a a) a)"
    (LambdaApplication 
      [
        (LambdaApplication [(LambdaId "a"), (LambdaId "a")]), 
        (LambdaId "a")
      ]
    )
  passLambda "pl14"
    "(a (a a))"
    (LambdaApplication 
      [
        (LambdaId "a"), 
        (LambdaApplication [(LambdaId "a"), (LambdaId "a")])
      ]
    )
  passLambda "pl15"
    "((a a) (a a))"
    (LambdaApplication 
      [
        (LambdaApplication [(LambdaId "a"), (LambdaId "a")]), 
        (LambdaApplication [(LambdaId "a"), (LambdaId "a")])
      ]
    )
  passLambda "pl16"
    "(/ b (a a))"
    (LambdaAbstraction 
      (LambdaId "b")
      (LambdaApplication 
        [
          (LambdaId "a"), 
          (LambdaId "a")
        ]
      )
    )
  passLambda "pl17"
    "(/ b (/ b a))"
    (LambdaAbstraction 
      (LambdaId "b") 
      (LambdaAbstraction 
        (LambdaId "b") 
        (LambdaId "a")
      )
    )
  passLambda "pl18"
    "(((((a a)(a a)) a) a)(a a))"
    (LambdaApplication 
      [
        (LambdaApplication 
          [
            (LambdaApplication 
              [
                (LambdaApplication 
                  [
                    (LambdaApplication [(LambdaId "a"), (LambdaId "a")]), 
                    (LambdaApplication [(LambdaId "a"), (LambdaId "a")])
                  ]
                ), 
                (LambdaId "a")
              ]
            ), 
            (LambdaId "a")
          ]
        ), 
        (LambdaApplication [(LambdaId "a"), (LambdaId "a")])
      ]
    )
  passLambda "pl19"
    "((/ b ((/ b (/ b a)) a))(/ b a))"
    (LambdaApplication 
      [
        (LambdaAbstraction 
          (LambdaId "b") 
          (LambdaApplication 
            [
              (LambdaAbstraction 
                (LambdaId "b") 
                (LambdaAbstraction (LambdaId "b")  (LambdaId "a"))
              ), 
              (LambdaId "a")
            ]
          )
        ), 
        (LambdaAbstraction (LambdaId "b") (LambdaId "a"))
      ]
    )
  passLambda "pl20" "a" (LambdaId "a")
  passLambda "pl21"
    "(#1 #2)" 
    (LambdaApplication 
      [(LambdaArgRef 1), 
      (LambdaArgRef 2)]
    )
  passLambda "pl22"
    "(% #1)" 
    (LambdaAnonAbstraction (LambdaArgRef 1))
  passLambda "pl23"
    "((#2 #1) #3)" 
    (LambdaApplication 
      [(LambdaApplication 
        [(LambdaArgRef 2), 
        (LambdaArgRef 1)]
      ),
      (LambdaArgRef 3)]
    )
  passLambda "pl24"
    "(#2 #1 #3)" 
    (LambdaApplication 
      [
        (LambdaArgRef 2), 
        (LambdaArgRef 1), 
        (LambdaArgRef 3)
      ]
    )
  passLambda "pl25"
    "(#4 (#6 #6))" 
    (LambdaApplication 
      [(LambdaArgRef 4), 
      (LambdaApplication 
        [(LambdaArgRef 6), 
        (LambdaArgRef 6)]
      )]
    )
  passLambda "pl26"
    "((#1 #2) (#2 #1))" 
    (LambdaApplication 
      [(LambdaApplication 
        [(LambdaArgRef 1), 
        (LambdaArgRef 2)]
      ), 
      (LambdaApplication 
        [(LambdaArgRef 2), 
        (LambdaArgRef 1)]
      )]
    )
  passLambda "pl27"
    "(#1 #2 (#2 #1))" 
    (LambdaApplication 
      [ 
        (LambdaArgRef 1), 
        (LambdaArgRef 2), 
        (LambdaApplication 
          [(LambdaArgRef 2), 
          (LambdaArgRef 1)]
        )
      ]
    )
  passLambda "pl28"
    "(% (#2 #2))" 
    (LambdaAnonAbstraction 
      (LambdaApplication 
        [(LambdaArgRef 2), 
        (LambdaArgRef 2)]
      )
    )
  passLambda "pl29"
    "(% (% #1))" 
    (LambdaAnonAbstraction 
      (LambdaAnonAbstraction 
        (LambdaArgRef 1)
      )
    )
  passLambda "pl30"
    "(((((#1 #1)(#1 #1)) #1) #1)(#1 #1))" 
    (LambdaApplication 
      [(LambdaApplication 
        [(LambdaApplication 
          [(LambdaApplication 
            [(LambdaApplication 
              [(LambdaArgRef 1), 
              (LambdaArgRef 1)]
            ), 
            (LambdaApplication 
              [(LambdaArgRef 1),
              (LambdaArgRef 1)]
            )]
          ), 
          (LambdaArgRef 1)]
        ), 
        (LambdaArgRef 1)]
      ), 
      (LambdaApplication 
        [(LambdaArgRef 1), 
        (LambdaArgRef 1)]
      )]
    )
  passLambda "pl31"
    "((% ((% (% #1)) #1))(% #1))" 
    (LambdaApplication 
      [(LambdaAnonAbstraction 
        (LambdaApplication 
          [(LambdaAnonAbstraction 
            (LambdaAnonAbstraction 
              (LambdaArgRef 1)
            )
          ), 
          (LambdaArgRef 1)]
        )
      ), 
      (LambdaAnonAbstraction (LambdaArgRef 1))]
    )
  passLambda "pl32" "(/ b a)" (LambdaAbstraction (LambdaId "b") (LambdaId "a"))
  
  failLambda "fl0" "(1 a)"
  failLambda "fl1" "(/ a b c)"
  failLambda "fl2" "(/ 1 a)" 
  failLambda "fl3" "(/ b 1)"
  failLambda "fl4" ""
  failLambda "fl5" "()"
  failLambda "fl6" "(/)"
  failLambda "fl7" "(())"
  failLambda "fl8" "(() ())"
  failLambda "fl9" "(/ ())"
  failLambda "fl10" "(/ (/))"
  failLambda "fl11" "((((()())))())"
  failLambda "fl12" "((/((/(/))))(/))"
  failLambda "fl13" "(/ a)"
  failLambda "fl14" "(/ (a a))"
  failLambda "fl15" "(/ (/ a))"
  failLambda "fl16" "((/ ((/ (/ a)) a))(/ a))"
  failLambda "fl17" "(/ b)"
  failLambda "fl18" "(/ b ())"
  failLambda "fl19" "(/ b (/ b))"
  failLambda "fl20" "((/ b ((/ b (/ b))))(/ b))"
  failLambda "fl21" ""
  failLambda "fl22" "()"
  failLambda "fl23" "(/)"
  failLambda "fl24" "(())"
  failLambda "fl25" "(() ())"
  failLambda "fl26" "(/ ())"
  failLambda "fl27" "(/ (/))"
  failLambda "fl28" "((((()())))())"
  failLambda "fl29" "((/((/(/))))(/))"
  failLambda "fl30" "(/ b)"
  failLambda "fl31" "(/ b ())"
  failLambda "fl32" "(/ b (/ b))"
  failLambda "fl33" "((/ b ((/ b (/ b))))(/ b))"