{-# OPTIONS_GHC -Wall #-}

module Lambda where

data LambdaValInput =
  LIV String |
  LIAB String LambdaValInput |
  LIAP LambdaValInput LambdaValInput
  deriving(Show, Eq)

data LambdaValAnon =
  LAR Int |
  LAB LambdaValAnon |
  LAP LambdaValAnon LambdaValAnon
  deriving(Show, Eq)


honk :: [LambdaValAnon] -> [LambdaValAnon]
honk orig = 
  [LAB x | x <- orig] ++ 
  [LAP x y | x <- orig, y <- orig] ++ 
  orig ++ 
  [ LAB (transformLambdaArgRefs (\argRef -> LAR (argRef+1)) x) | x <- orig]

transformLambda :: (Int -> LambdaValAnon) -> (LambdaValAnon -> LambdaValAnon) -> (LambdaValAnon -> LambdaValAnon -> LambdaValAnon) -> LambdaValAnon -> LambdaValAnon
transformLambda tranformArgRef _ _ (LAR argRef) = tranformArgRef argRef
transformLambda _ tranformAbstraction _ (LAB body) = tranformAbstraction body
transformLambda _ _ transformLambdaApplication (LAP func body) = transformLambdaApplication func body

transformLambdaArgRefs :: (Int -> LambdaValAnon) -> LambdaValAnon -> LambdaValAnon
transformLambdaArgRefs tranformArgRef = transformLambda tranformArgRef (LAB . (transformLambdaArgRefs tranformArgRef)) hurger
  where
    hurger func arg = 
      (LAP 
        (transformLambdaArgRefs tranformArgRef func)
        (transformLambdaArgRefs tranformArgRef arg)
      )

transformLambdaAbstractions :: (LambdaValAnon -> LambdaValAnon) -> LambdaValAnon -> LambdaValAnon
transformLambdaAbstractions _ lar@(LAR _) = 
  lar
transformLambdaAbstractions transform (LAB body) =
  transform body
transformLambdaAbstractions transform (LAP func arg) =
  (LAP 
    (transformLambdaAbstractions transform func)
    (transformLambdaAbstractions transform arg)
  )

transformLambdaApplications :: (LambdaValAnon -> LambdaValAnon -> LambdaValAnon) -> LambdaValAnon -> LambdaValAnon
transformLambdaApplications _ lar@(LAR _) = 
  lar
transformLambdaApplications transform (LAB body) =
  (LAB (transformLambdaApplications transform body))
transformLambdaApplications transform (LAP func arg) =
  transform func arg

lambdaBoundVarsAnonymized :: LambdaValInput -> Maybe LambdaValAnon
lambdaBoundVarsAnonymized = lambdaVarReplacedWithArgRefs []

incReplacements :: [(String, Int)] -> [(String, Int)]
incReplacements = map ((,) <$> fst <*> (+1) . snd)

lambdaVarReplacedWithArgRefs :: [(String, Int)] -> LambdaValInput -> Maybe LambdaValAnon
lambdaVarReplacedWithArgRefs replacements (LIV str) = do
  replacement <- lookup str replacements
  return (LAR replacement)
lambdaVarReplacedWithArgRefs replacements (LIAB str body) = do
  let newReplacements = (str, 1):(incReplacements replacements)
  newBody <- lambdaVarReplacedWithArgRefs newReplacements body
  return (LAB newBody)
lambdaVarReplacedWithArgRefs replacements (LIAP func arg) = do
  newFunc <- lambdaVarReplacedWithArgRefs replacements func
  newArg <- lambdaVarReplacedWithArgRefs replacements arg
  return (LAP newFunc newArg)



lambdaBetaReducedOneStep :: LambdaValAnon -> LambdaValAnon
lambdaBetaReducedOneStep (LAB val) = 
  (LAB (lambdaBetaReducedOneStep val))
lambdaBetaReducedOneStep (LAP (LAB func) arg) =
  lambdaAppliedTo arg func
lambdaBetaReducedOneStep lap@(LAP func arg)
  | funcReducedOnce /= func = (LAP funcReducedOnce arg)
  | argReducedOnce /= arg = (LAP func argReducedOnce)
  | otherwise = lap
  where
    funcReducedOnce = lambdaBetaReducedOneStep func
    argReducedOnce = lambdaBetaReducedOneStep arg
lambdaBetaReducedOneStep lar@(LAR _) =
  lar

lambdaAppliedTo :: LambdaValAnon -> LambdaValAnon -> LambdaValAnon
lambdaAppliedTo = lambdaArgRefReplacedWithLambda 1

transformArgRefReplacedWithLambda :: Int -> LambdaValAnon -> Int -> LambdaValAnon
transformArgRefReplacedWithLambda argRefReplace arg argRef
  | argRef == 1 && argRefReplace == 1 = arg
  | argRefReplace == 1 = (LAR (argRef - 1))
  | argRefReplace == argRef = lambdaIncrementedArgRefsGreaterThan arg 1 argRef
  | otherwise = (LAR argRef)

lambdaArgRefReplacedWithLambda :: Int -> LambdaValAnon -> LambdaValAnon -> LambdaValAnon
lambdaArgRefReplacedWithLambda argRefReplace arg = 
  transformLambdaArgRefs (transformArgRefReplacedWithLambda argRefReplace arg) 

lambdaIncrementedArgRefsGreaterThan :: LambdaValAnon -> Int -> Int -> LambdaValAnon
lambdaIncrementedArgRefsGreaterThan lar@(LAR argRef) argRefPatchMin argRefReplacing
  | argRef <= argRefPatchMin = lar
  | otherwise = (LAR (argRef + argRefReplacing - 1))
lambdaIncrementedArgRefsGreaterThan (LAB func) argRefPatchMin argRefReplacing =
  (LAB (lambdaIncrementedArgRefsGreaterThan func argRefPatchMin argRefReplacing))
lambdaIncrementedArgRefsGreaterThan (LAP func arg) argRefPatchMin argRefReplacing =
  (LAP 
    (lambdaIncrementedArgRefsGreaterThan func argRefPatchMin argRefReplacing)
    (lambdaIncrementedArgRefsGreaterThan arg argRefPatchMin argRefReplacing)
  )

foo :: LambdaValInput
foo = 
  (LIAB "x" 
    (LIAP
      (LIAB "y" (LIV "y"))
      (LIV "x")
    )
  )

bar :: Maybe LambdaValAnon
bar = lambdaBoundVarsAnonymized foo

bing :: LambdaValAnon
bing = maybe (LAR (-1)) lambdaBetaReducedOneStep bar