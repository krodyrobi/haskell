import Debug.Trace
import Data.Set (Set)
import qualified Data.Set as Set

data Val = IntV Integer
         | FloatV Float
         | CharV Char
         | StringV String
         | BooleanV Bool
         -- since we are implementing a Functional language, functions are
         -- first class citizens.
         | LambdaV [String] Expr Env
         
         -- Internal use only
         | RecMarker Expr
     deriving (Show, Eq)
  
-----------------------------------------------------------
data Expr = 
            Const Val
          -- represents a variable
          | Var String
          -- integer multiplication
          | Expr :*: Expr 
          -- integer addition and string concatenation
          | Expr :+: Expr 
          | Expr :-: Expr 
          -- equality test. Defined for all Val except FunVal
          | Expr :==: Expr 
          -- semantically equivalent to a Haskell `if`
          | If Expr Expr Expr
         
          -- binds a Var (the first `Expr`) to a value (the second `Expr`), 
          -- and makes that binding available in the third expression
          | Let String Expr Expr
         
          -- similar to Let but allows for recursive definitions. You can also make
          -- the above Let support this, and simply remove this expression. But since
          -- implementing a recursive definition of Let is a bit trickier, you will want
          -- at least implement a non-recursive Let.
          | Letrec String Expr Expr
         
          -- creates an anonymous function with an arbitrary number of parameters
          | Lambda [String] Expr 
         
          -- calls a function with an arbitrary number values for parameters
          | Apply Expr [Expr]
     deriving (Show, Eq)

-----------------------------------------------------------

--you can replace this with whatever you think will solve your problem. 
--e.g. with a Map data structure (or a stack of maps) from the standard 
--Haskell library, and so on.
data Env = EmptyEnv
         | ExtendEnv String Val Env
     deriving Eq

-- can't follow the default show had to do it :D
instance Show Env where
  show EmptyEnv = "\r\n   -"
  show (ExtendEnv name val env) = "\r\n  " ++ name ++ " = " ++ (show val) ++ (show env)

find :: Env -> String -> Val
find env name = case env of
  ExtendEnv storedName value nextEnv -> if storedName == name then value else find nextEnv name
  EmptyEnv -> error $ "Variable not bound to a value: " ++ name

  
add :: Env -> String -> Val -> Env
add env name value = ExtendEnv name value env


-----------------------------------------------------------

-- the evaluate function takes an environment, which holds variable
-- bindings; i.e. it stores information like `x = 42`
-- the trace there will print out the values with which the function was called,
-- you can easily uncomment it if you don't need it for debugging anymore.
evaluate:: Expr -> Env -> Val
evaluate expr env = 
  trace("expr= " ++ show expr ++ "\r\n env=" ++ show env) $
  case expr of
  Const v  -> v
  Var name -> case find env name of
                RecMarker expr -> evaluate expr env
                val -> val
  e@(lhs :+: rhs) ->
    let valLhs = evaluate lhs env
        valRhs = evaluate rhs env
    in case (valLhs, valRhs) of
        (IntV v1, IntV v2) -> IntV $ v1 + v2
        (FloatV v1, FloatV v2) -> FloatV $ v1 + v2
        (StringV s1, StringV s2) -> StringV $ s1 ++ s2
        args@(_, _) -> error $ "The addition only works on ints/floats or strings " ++ show args
  lhs :-: rhs -> 
    let valLhs = evaluate lhs env
        valRhs = evaluate rhs env
    in case (valLhs, valRhs) of
        (IntV v1, IntV v2) -> IntV $ v1 - v2
        (FloatV v1, FloatV v2) -> FloatV $ v1 - v2
        args@(_, _) -> error $ "The substraction only works on ints/floats " ++ show args
  lhs :*: rhs -> 
    let valLhs = evaluate lhs env
        valRhs = evaluate rhs env
    in case (valLhs, valRhs) of
        (IntV v1, IntV v2) -> IntV $ v1 * v2
        (FloatV v1, FloatV v2) -> FloatV $ v1 * v2
        args@(_, _) -> error $ "The multiplication only works on ints or floats " ++ show args
  lhs :==: rhs -> 
    let valLhs = evaluate lhs env
        valRhs = evaluate rhs env
    in case (valLhs, valRhs) of
        (IntV v1, IntV v2) -> BooleanV $ v1 == v2
        (FloatV v1, FloatV v2) -> BooleanV $ v1 == v2
        (StringV v1, StringV v2) -> BooleanV $ v1 == v2
        (CharV v1, CharV v2) -> BooleanV $ v1 == v2
        (BooleanV v1, BooleanV v2) -> BooleanV $ v1 == v2
        args@(_, _) -> error $ "Equality not defined " ++ show args
  If cond trueExpr falseExpr ->
    let valCond = evaluate cond env
    in case valCond of
      BooleanV True -> evaluate trueExpr env
      BooleanV False -> evaluate falseExpr env
      _ -> error $ "First expression in if should evaluate to BooleanV " ++ show valCond
  -- TODO think about letrec.. 
  Letrec name expr expr2 ->
    evaluate expr2 new_env
    where new_env = add env name (RecMarker expr)
  Let name expr expr2 ->
    evaluate expr2 (add env name (evaluate expr env))
  Lambda args body -> LambdaV args body env
  Apply (Lambda names body) argExprs ->
    case (names, argExprs) of
      ([], []) -> evaluate body env
      ([], e@[_]) -> error $ "Lambda wasn't expecting more values " ++ show e
      (args@[_], []) -> LambdaV args body env
      (n:ns, e:es) -> evaluate (Apply (Lambda ns body) es) (add env n (evaluate e env))
  Apply object argExprs ->
    let val = evaluate object env
    in case val of
      LambdaV lambda_args lambda_body lambda_env -> evaluate (Apply (Lambda lambda_args lambda_body) argExprs) lambda_env
      e@_ -> error $ "Var does not contain a LambdaV got: " ++ show e


-----------------------------------------------------------
-- the function that we test. since we always start out with an EmptyEnv.
interpret :: Expr -> Val
interpret expr = evaluate expr EmptyEnv














---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------- Tests ----------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
testConstant = 
  assert result (IntV 2) "testConstant"
  where result = interpret expr
        expr = Const (IntV 2)

testSubst = 
  assert result (IntV 0) "testSubst"
  where result = interpret expr
        expr = (Const (IntV 1)) :-: (Const (IntV 1))

testSubstVar = 
  assert result (IntV 0) "testSubstVar"
  where result = evaluate expr env
        expr = (Var "a") :-: (Var "b")
        env = add (add EmptyEnv "a" (IntV 1)) "b" (IntV 1)

testSubstFloats = 
  assert result (FloatV 0.0) "testSubstFloats"
  where result = interpret expr
        expr = (Const (FloatV 1.0)) :-: (Const (FloatV 1.0))


testAddition = 
  assert result (IntV 2) "testAddition"
  where result = interpret expr
        expr = (Const (IntV 1)) :+: (Const (IntV 1))

testAdditionVar = 
  assert result (IntV 2) "testAdditionVar"
  where result = evaluate expr env
        expr = (Var "a") :+: (Var "b")
        env = add (add EmptyEnv "a" (IntV 1)) "b" (IntV 1)

testAdditionFloats = 
  assert result (FloatV 2.0) "testAdditionFloats"
  where result = interpret expr
        expr = (Const (FloatV 1.0)) :+: (Const (FloatV 1.0))

testConcatenation = 
  assert result (StringV "abcd") "testConcatenation"
  where result = interpret expr
        expr = (Const (StringV "ab")) :+: (Const (StringV "cd"))


testMultiplication = 
  assert result (IntV 6) "testMultiplication"
  where result = interpret expr
        expr = (Const (IntV 3)) :*: (Const (IntV 2)) 

testMultiplicationFloats = 
  assert result (FloatV 6.0) "testMultiplicationFloats"
  where result = interpret expr
        expr = (Const (FloatV 3.0)) :*: (Const (FloatV 2.0))
        
testEqualityInts =
  assert result (BooleanV True) "testEqualityInts"
  where result = interpret expr
        expr = (Const (IntV 3)) :==: (Const (IntV 3))

testEqualityIntsNot =
  assert result (BooleanV False) "testEqualityIntsNot"
  where result = interpret expr
        expr = (Const (IntV 4)) :==: (Const (IntV 3))

testEqualityFloats =
  assert result (BooleanV True) "testEqualityFloats"
  where result = interpret expr
        expr = (Const (FloatV 3.0)) :==: (Const (FloatV 3.0))

testEqualityFloatsNot =
  assert result (BooleanV False) "testEqualityFloatsNot"
  where result = interpret expr
        expr = (Const (FloatV 4.0)) :==: (Const (FloatV 3.0))

testEqualityStrings =
  assert result (BooleanV True) "testEqualityStrings"
  where result = interpret expr
        expr = (Const (StringV "a")) :==: (Const (StringV "a"))

testEqualityStringsNot =
  assert result (BooleanV False) "testEqualityStringsNot"
  where result = interpret expr
        expr = (Const (StringV "a")) :==: (Const (StringV "b"))
        
testEqualityChars =
  assert result (BooleanV True) "testEqualityChars"
  where result = interpret expr
        expr = (Const (CharV 'a')) :==: (Const (CharV 'a'))

testEqualityCharsNot =
  assert result (BooleanV False) "testEqualityCharsNot"
  where result = interpret expr
        expr = (Const (CharV 'a')) :==: (Const (CharV 'b'))
        
testEqualityBooleans =
  assert result (BooleanV True) "testEqualityBooleans"
  where result = interpret expr
        expr = (Const (BooleanV True)) :==: (Const (BooleanV True))
        
testEqualityBooleansNot =
  assert result (BooleanV False) "testEqualityBooleansNot"
  where result = interpret expr
        expr = (Const (BooleanV False)) :==: (Const (BooleanV True))

testIfTrue =
  assert result (BooleanV True) "testIfTrue"
  where result = interpret expr
        expr = If (Const (BooleanV True)) (Const (BooleanV True)) (Const (BooleanV False))

testIfFalse =
  assert result (BooleanV False) "testIfFalse"
  where result = interpret expr
        expr = If (Const (BooleanV False)) (Const (BooleanV True)) (Const (BooleanV False))

testFind = 
  assert result (BooleanV False) "testFind"
  where result = find env "test"
        env = ExtendEnv "test" (BooleanV False) EmptyEnv

testFindNested = 
  assert result (BooleanV False) "testFindNested"
  where result = find env "test"
        env = ExtendEnv "test" (BooleanV False) inner_env
        inner_env = ExtendEnv "test" (BooleanV True) EmptyEnv

testAdd =
  assert result (BooleanV False) "testFindNested"
  where result = find env "test"
        env = add EmptyEnv "test" (BooleanV False)

testAddNested =
  assert result (BooleanV False) "testFindNested"
  where result = find env "test"
        env = add inner_env "test" (BooleanV False)
        inner_env = add EmptyEnv "test" (BooleanV True)
        
testVar = 
  assert result (BooleanV False) "testVar"
  where result = evaluate expr env
        expr = Var "test"
        env = ExtendEnv "test" (BooleanV False) EmptyEnv
        
testLet =
  assert result (BooleanV False) "testLet"
  where result = evaluate expr env
        env = ExtendEnv "a" (BooleanV True) EmptyEnv
        expr = Let "b" (Const (BooleanV False)) ((Var "a") :==: (Var "b"))
        
testLambda = 
  assert result (IntV 7) "testLambda"
  where result = evaluate expr EmptyEnv
        expr = Apply (Lambda ["a", "b"] (Var "a" :+: Var "b")) [Const (IntV 4), Const (IntV 3)]
        
testCurying = 
  assert result (IntV 7) "testCurying"
  where result = evaluate expr EmptyEnv
        expr = Let "func" (Lambda ["a", "b"] (Var "a" :+: Var "b")) (Apply (Apply (Var "func") [Const (IntV 4)]) [Const (IntV 3)])

testCuryingPartialApplication = 
  assert result (LambdaV ["b"] (Var "b") (add EmptyEnv "a" (IntV 1))) "testCuryingPartialApplication"
  where result = evaluate (Apply (Lambda ["a", "b"] (Var "b")) [Const (IntV 1)]) EmptyEnv

letRecSimpleExpr = (Letrec "countDownTo0"
  (Lambda ["n"]
    (If 
      ((Var "n") :==: (Const (IntV 0)))
      ((Const (IntV 0)))
      (Apply (Var "countDownTo0") [Var "n" :-: (Const (IntV 1))])
    ))
  (Apply (Var "countDownTo0") [(Const (IntV 2))]))

testRecSimple =
  assert result (IntV 0) "testRecSimple"
  where result = evaluate letRecSimpleExpr EmptyEnv
        
letRecExpr x = (Letrec 
               "fibb"
               --this means that this lambda will be called "fibb"
               fibbonaciLambda
               -- in: "fibb 5", i.e. the 5th fibonnaci number, 
               (applyFib (constInt x))
              )
  where constInt x = (Const $ IntV x)
        
        compareNToX x = (Var "n") :==: (constInt x)

        --remember that we construct the AST, so we can "write ahead" assuming that the variable "fibb"
        --will be bound to the variable "fibb"
        applyFib :: Expr -> Expr
        applyFib p = Apply (Var "fibb") [p]

        nMinus x = (Var "n") :-: (Const (IntV x)) 
        -- an equivalent of nth fibbonacci number anonymous lambda function
        fibbonaciLambda = (Lambda 
            ["n"]
            (If (compareNToX 0)
              --then branch if n == 0
                (constInt 0)
              --else branch if it is equal to 1
                (If (compareNToX 1)
                  --then branch if n == 1
                  (Const $ IntV 1)
                  -- else branch that does computation, and does recursive call, 
                  -- assuming that in the Let construction below we name the function "fibb"  
                  (applyFib (nMinus 1)) :+: (applyFib (nMinus 2))  
                )
             )
          )
          
letRecExprStar x = (Letrec 
               "fibb"
               --this means that this lambda will be called "fibb"
               fibbonaciLambda
               -- in: "fibb 5", i.e. the 5th fibonnaci number, 
               (applyFib (constInt x))
              )
  where constInt x = (Const $ IntV x)
        
        compareNToX x = (Var "n") :==: (constInt x)

        --remember that we construct the AST, so we can "write ahead" assuming that the variable "fibb"
        --will be bound to the variable "fibb"
        applyFib :: Expr -> Expr
        applyFib p = Apply (Var "fibb") [p]

        nMinus x = (Var "n") :-: (Const (IntV x)) 
        -- an equivalent of nth fibbonacci number anonymous lambda function
        fibbonaciLambda = (Lambda 
            ["n"]
            (If (compareNToX (-1))
              (constInt 0)
              (If (compareNToX 0)
                --then branch if n == 0
                (constInt 0)
                --else branch if it is equal to 1
                (If (compareNToX 1)
                  --then branch if n == 1
                  (Const $ IntV 1)
                  -- else branch that does computation, and does recursive call, 
                  -- assuming that in the Let construction below we name the function "fibb"  
                  (applyFib (nMinus 1)) :+: (applyFib (nMinus 2))  
                )
              )
            )
          )



testLetrec = 
  assert result (IntV 5) "testLetrec"
  where result = interpret (letRecExpr 5)

testLetrecStar = 
  assert result (IntV 5) "testLetrecStar"
  where result = interpret (letRecExprStar 5)

testAll = 
  if (allPassed) 
    then "All tests passed."
    else error "Failed tests."
  where allPassed = testConstant &&
                    testSubst &&
                    testSubstFloats &&
                    testSubstVar &&
                    testAddition &&
                    testAdditionFloats &&
                    testAdditionVar &&
                    testConcatenation &&
                    testMultiplication &&
                    testMultiplicationFloats &&
                    testEqualityInts &&
                    testEqualityIntsNot &&
                    testEqualityFloats &&
                    testEqualityFloatsNot &&
                    testEqualityStrings &&
                    testEqualityStringsNot &&
                    testEqualityChars &&
                    testEqualityCharsNot &&
                    testEqualityBooleans &&
                    testEqualityBooleansNot &&
                    testIfTrue &&
                    testIfFalse &&
                    testFind &&
                    testFindNested &&
                    testAdd &&
                    testAddNested &&
                    testVar &&
                    testLet &&
                    testLambda &&
                    testRecSimple &&
                    testLetrecStar &&
                 --   testLetrec &&
                    testCurying &&
                    testCuryingPartialApplication

{-
If I remove the inner if n == -1 then this happens even though the env seems just right

n=1 ->

If (Var "n" :==: Const (IntV 1))
  (Const (IntV 1))
  (Apply (Var "fibb") [Var "n" :-: Const (IntV 1)]) :+: Apply (Var "fibb") [Var "n" :-: Const (IntV 2)]

  
If (Var "n" :==: Const (IntV 1))
  (Const (IntV 1))
  (Apply (Var "fibb") [Var "n" :-: Const (IntV 1)])

Don't understand the second step why it happens but the thing is after
it does instead of going for returning const intv 1 it evals the apply even though
the if rule is not executing them both.

To see the trace run interpret (letRecExpr 1) and stop asap.
-}
                    -- TODO Haskell try catch? simple example
---------------------------------------------------------------------
assert :: Val -> Val -> String -> Bool
assert expected received message = 
  if (expected == received) 
    then True
    else error $ message ++ " -> expected: `" ++ (show expected) ++ "`; received: `" ++ (show received) ++ "`"