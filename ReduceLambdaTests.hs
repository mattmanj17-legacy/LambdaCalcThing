
module ReduceLambdaTests 
(
  reduceLambdaTests
) 
where

import Data.Either
import Test.Hspec

import LambdaAst
import ReduceLambda
import ParseLambda
import ParseCommon

reduceFullTest :: String -> String -> String -> SpecWith ()
reduceFullTest strDesc strIn strOut = do
  it strDesc $ do
    parsedIn `shouldSatisfy` isRight
  it strDesc $ do
    parsedOut `shouldSatisfy` isRight
  if isRight parsedIn && isRight parsedOut then do
    it strDesc $ do
      reducedOnce `shouldSatisfy` isRight
    if isRight reducedOnce then
      it strDesc $ do
        justReducedOnced `shouldBe` justOut
    else
      return ()
  else
    return ()
  where
    parsedIn = parseFromStrToEither parseLambda strIn
    parsedOut = parseFromStrToEither parseLambda strOut
    justIn = fromRightUnsafe parsedIn
    justOut = fromRightUnsafe parsedOut
    reducedOnce = lambdaBetaReducedFull justIn
    justReducedOnced = fromRightUnsafe reducedOnce

churchNum :: Int -> LambdaAst
churchNum n = (LambdaAnonAbstraction (LambdaAnonAbstraction (churchNumHelper n)))
  where
    churchNumHelper 0 = (LambdaArgRef 1)
    churchNumHelper m = (LambdaApplication [(LambdaArgRef 2), (churchNumHelper (m-1))])

intUnaryOpTest :: String -> LambdaAst -> (Int -> Int) -> Int -> SpecWith ()
intUnaryOpTest strDesc expr op a = do
  it strDesc $ do 
    reduced `shouldSatisfy` isRight
  if isRight reduced then
    it strDesc $ do 
      justReduced `shouldBe` (churchNum (op a))
  else
    return ()
  where
    reduced = lambdaBetaReducedFull (LambdaApplication [expr, (churchNum a)])
    justReduced = fromRightUnsafe reduced

succOperator = 
  (LambdaAnonAbstraction 
    (LambdaAnonAbstraction 
      (LambdaAnonAbstraction 
        (LambdaApplication 
          [(LambdaArgRef 2), 
          (LambdaApplication 
            [(LambdaApplication 
              [(LambdaArgRef 3), 
              (LambdaArgRef 2)]
            ), 
            (LambdaArgRef 1)]
          )]
        )
      )
    )
  )
succExprTest = intUnaryOpTest "succ" succOperator (+1)

predOperator = 
  (LambdaAnonAbstraction 
    (LambdaAnonAbstraction 
      (LambdaAnonAbstraction 
        (LambdaApplication 
          [(LambdaApplication 
            [(LambdaApplication 
              [(LambdaArgRef 3), 
              (LambdaAnonAbstraction 
                (LambdaAnonAbstraction 
                  (LambdaApplication 
                    [(LambdaArgRef 1), 
                    (LambdaApplication 
                      [(LambdaArgRef 2), 
                      (LambdaArgRef 4)]
                    )]
                  )
                )
              )]
            ), 
            (LambdaAnonAbstraction 
              (LambdaArgRef 2)
            )]
          ), 
          (LambdaAnonAbstraction 
            (LambdaArgRef 1)
          )]
        )
      )
    )
  )
predExprTest = intUnaryOpTest "pred" predOperator (\n -> if n == 0 then 0 else (n-1))

intBinaryOpTest :: String -> LambdaAst -> (Int -> Int -> Int) -> Int -> Int -> SpecWith ()
intBinaryOpTest strDesc expr op a b =
  it strDesc $ do 
    shouldBe
      (lambdaBetaReducedFull 
        (LambdaApplication 
          [(LambdaApplication [expr, (churchNum a)]), 
          (churchNum b)]
        )
      )  
      (Right (churchNum (op a b)))

addOperator = 
  (LambdaAnonAbstraction 
    (LambdaAnonAbstraction 
      (LambdaAnonAbstraction 
        (LambdaAnonAbstraction 
          (LambdaApplication 
            [(LambdaApplication 
              [(LambdaArgRef 4), 
              (LambdaArgRef 2)]
            ),
            (LambdaApplication 
              [(LambdaApplication 
                [(LambdaArgRef 3), 
                (LambdaArgRef 2)]
              ), 
              (LambdaArgRef 1)]
            )]
          )
        )
      )
    )
  )
addExprTest = intBinaryOpTest "add" addOperator (+)

multOperator = 
  (LambdaAnonAbstraction 
    (LambdaAnonAbstraction 
      (LambdaAnonAbstraction 
        (LambdaApplication 
          [(LambdaArgRef 3) ,
          (LambdaApplication 
            [(LambdaArgRef 2), 
            (LambdaArgRef 1)]
          )]
        )
      )
    )
  )
multExprTest = intBinaryOpTest "mul" multOperator (*)

powOperator = 
  (LambdaAnonAbstraction 
    (LambdaAnonAbstraction 
      (LambdaAnonAbstraction 
        (LambdaAnonAbstraction 
          (LambdaApplication 
            [(LambdaApplication 
              [(LambdaApplication 
                [(LambdaArgRef 3), 
                (LambdaArgRef 4)]
              ), 
              (LambdaArgRef 2)]
            ), 
            (LambdaArgRef 1)]
          )
        )
      )
    )
  )
powExprTest = intBinaryOpTest "pow" powOperator (^)

subOperator = 
  (LambdaAnonAbstraction 
    (LambdaAnonAbstraction 
      (LambdaApplication 
        [(LambdaApplication 
          [(LambdaArgRef 1), 
          predOperator]
        ), 
        (LambdaArgRef 2)]
      )
    )
  )
subExprTest = intBinaryOpTest "sub" subOperator (\a b -> if a < b then 0 else a - b)

reduceLambdaTests = do
  
  it "ro0" $ do 
    shouldNotSatisfy
      (lambdaBetaReducedOneStep (fromRightUnsafe (parseFromStrToEither parseLambda "((% (/ #1 a)) s)")))
      isRight

  it "ro1" $ do 
    shouldNotSatisfy
      (lambdaBetaReducedOneStep (fromRightUnsafe (parseFromStrToEither parseLambda "((% (/ (/ b b) a)) s)")))
      isRight

  it "ro2" $ do 
    shouldNotSatisfy
      (lambdaBetaReducedOneStep (fromRightUnsafe (parseFromStrToEither parseLambda "((% (/ (% #1) a)) s)")))
      isRight

  it "ro3" $ do 
    shouldNotSatisfy
      (lambdaBetaReducedOneStep (fromRightUnsafe (parseFromStrToEither parseLambda "((% (/ [] a)) s)")))
      isRight

  it "ro4" $ do 
    shouldNotSatisfy
      (lambdaBetaReducedOneStep (fromRightUnsafe (parseFromStrToEither parseLambda "((% (/ (b b) a)) s)")))
      isRight

  it "ro5" $ do 
    shouldNotSatisfy
      (lambdaBetaReducedOneStep (fromRightUnsafe (parseFromStrToEither parseLambda "(a)")))
      isRight

  reduceFullTest "rf1" "(cons a [b c])" "[a b c]"
  reduceFullTest "rf2" "(apply [a b c])" "(a b c)" -- this can probably be implimented at user level
  reduceFullTest "rf3" "(fn a a)" "(% #1)"
  reduceFullTest "rf4" "(cons a [])" "[a]"
  reduceFullTest "rf5" "(letin [a f] (a b c d e))" "(f b c d e)"
  reduceFullTest "rf6" "(letin [[a f] [b g]] (a b c d e))" "(f g c d e)"

  reduceFullTest "rf5" "(letin ((/ s [s f]) a) (a b c d e))" "(f b c d e)"

  succExprTest 0
  succExprTest 1
  succExprTest 2
  succExprTest 3
  succExprTest 4

  predExprTest 0
  predExprTest 1
  predExprTest 2
  predExprTest 3
  predExprTest 4

  addExprTest 0 0
  addExprTest 0 1
  addExprTest 0 2
  addExprTest 0 3
  addExprTest 0 4
  addExprTest 1 0
  addExprTest 1 1
  addExprTest 1 2
  addExprTest 1 3
  addExprTest 1 4
  addExprTest 2 0
  addExprTest 2 1
  addExprTest 2 2
  addExprTest 2 3
  addExprTest 2 4
  addExprTest 3 0
  addExprTest 3 1
  addExprTest 3 2
  addExprTest 3 3
  addExprTest 3 4
  addExprTest 4 0
  addExprTest 4 1
  addExprTest 4 2
  addExprTest 4 3
  addExprTest 4 4

  multExprTest 0 0
  multExprTest 0 1
  multExprTest 0 2
  multExprTest 0 3
  multExprTest 0 4
  multExprTest 1 0
  multExprTest 1 1
  multExprTest 1 2
  multExprTest 1 3
  multExprTest 1 4
  multExprTest 2 0
  multExprTest 2 1
  multExprTest 2 2
  multExprTest 2 3
  multExprTest 2 4
  multExprTest 3 0
  multExprTest 3 1
  multExprTest 3 2
  multExprTest 3 3
  multExprTest 3 4
  multExprTest 4 0
  multExprTest 4 1
  multExprTest 4 2
  multExprTest 4 3
  multExprTest 4 4

  powExprTest 0 0
  powExprTest 0 1
  powExprTest 0 2
  powExprTest 0 3
  powExprTest 0 4
  powExprTest 1 0
  powExprTest 1 1
  powExprTest 1 2
  powExprTest 1 3
  powExprTest 1 4
  powExprTest 2 0
  powExprTest 2 1
  powExprTest 2 2
  powExprTest 2 3
  powExprTest 2 4
  powExprTest 3 0
  powExprTest 3 1
  powExprTest 3 2
  powExprTest 3 3
  powExprTest 3 4
  powExprTest 4 0
  powExprTest 4 1
  powExprTest 4 2
  powExprTest 4 3
  powExprTest 4 4

  subExprTest 0 0
  subExprTest 0 1
  subExprTest 0 2
  subExprTest 0 3
  subExprTest 0 4
  subExprTest 1 0
  subExprTest 1 1
  subExprTest 1 2
  subExprTest 1 3
  subExprTest 1 4
  subExprTest 2 0
  subExprTest 2 1
  subExprTest 2 2
  subExprTest 2 3
  subExprTest 2 4
  subExprTest 3 0
  subExprTest 3 1
  subExprTest 3 2
  subExprTest 3 3
  subExprTest 3 4
  subExprTest 4 0
  subExprTest 4 1
  subExprTest 4 2
  subExprTest 4 3
  subExprTest 4 4