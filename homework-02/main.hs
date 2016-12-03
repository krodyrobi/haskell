import Debug.Trace
import qualified Data.Map as Map

data Type = TVar Integer
          | TArr Type Type
          | TInt
          | TBool
          deriving Eq

instance Show Type where
  show t = inner_show (standardize_type t)
    where inner_show t = case t of
            TInt -> "int"
            TBool -> "bool"
            TVar n -> "t" ++ show n
            TArr arg res -> inner_show arg ++ " -> " ++ inner_show res


data Val = IntVal Integer
         | BoolVal Bool
         | FunVal [String] Expr Env
         deriving (Show, Eq)

data Env = EmptyEnv
         | ExtendedEnv String Val Env
         deriving (Show, Eq)

data TEnv = EmptyTEnv
          | ExtendedTEnv String Type TEnv
          deriving (Show, Eq)

data Expr = Const Val
          | Var String
          | Expr :*: Expr
          | Expr :+: Expr
          | Expr :==: Expr
          | If Expr Expr Expr
          | Let String Expr Expr
          | Lambda [String] Expr
          | Apply Expr [Expr]
          deriving (Show, Eq)

type Subst  = Map.Map Integer Type
type Answer = (Type, Subst)

standardize_type :: Type -> Type
standardize_type t =
  -- can definitly be cleaned up but whatever :D
  let label store t =
        case t of
          TVar n -> case Map.lookup n store of
            Nothing -> Map.insert n (toInteger ((Map.size store) + 1)) store
            Just v  -> store
          TArr arg res -> label (label store arg) res
          TInt  -> store
          TBool -> store
      process store t =
        case t of
          TVar n -> case Map.lookup n store of
            Just v -> TVar v
            Nothing -> error "shouldn't happen"
          TArr arg res -> TArr (process store arg) (process store res)
          TInt -> TInt
          TBool -> TBool
  in process (label Map.empty t) t

apply_one_subst :: Type -> Type -> Type -> Type
apply_one_subst t0 tvar@(TVar n) t1 =
  -- replace tvar with t1 in t0
  case t0 of
    TInt         -> TInt
    TBool        -> TBool
    TVar _       -> if tvar == t0 then t1 else t0
    TArr arg res -> TArr (apply_one_subst arg tvar t1) (apply_one_subst res tvar t1)


apply_subst_to_type :: Type -> Subst -> Type
apply_subst_to_type t subst =
  case t of
    TInt         -> TInt
    TBool        -> TBool
    TVar n       -> case Map.lookup n subst of
                      Nothing -> t
                      Just tt -> tt
    TArr arg res -> TArr (apply_subst_to_type arg subst) (apply_subst_to_type res subst)


empty_subst :: Subst
empty_subst = Map.empty


extend_subst :: Subst -> Type -> Type -> Subst
extend_subst old_subst tvar@(TVar n) t =
  -- inserts new substitution and applies all others as well
  Map.insert n t (Map.map (\x -> apply_one_subst x tvar t) old_subst)


no_occurrence :: Type -> Type -> Bool
no_occurrence tvar@(TVar n) t =
  case t of
    TInt       -> True
    TBool      -> True
    TVar _     -> not (t == tvar)
    TArr t1 t2 -> no_occurrence tvar t1 && no_occurrence tvar t2


unifier :: Type -> Type -> Subst -> Expr -> Subst
unifier t1 t2 subst expr =
  if ty1 == ty2 then subst
  else
    case (ty1, ty2) of
      (TVar n, t) ->
        if no_occurrence (TVar n) t then extend_subst subst (TVar n) t
        else error $ "Unification failure no occurrence " ++ show (TVar n) ++ " appears in " ++ show t ++ " in expr= " ++ show expr
      (t, TVar n) ->
        if no_occurrence (TVar n) t then extend_subst subst (TVar n) t
        else error $ "Unification failure no occurrence " ++ show (TVar n) ++ " appears in " ++ show t ++ " in expr= " ++ show expr
      (TArr arg1 res1, TArr arg2 res2) ->
        unifier arg1 arg2 (unifier res1 res2 subst expr) expr
      _ -> error $ "Unification failure " ++ show ty1 ++ " != " ++ show ty2 ++ " in " ++ show expr
  where ty1 = apply_subst_to_type t1 subst
        ty2 = apply_subst_to_type t2 subst

-- TODO test the unifier


type_of :: Expr -> TEnv -> Subst -> Answer
type_of expr tenv subst = case expr of
  Const (IntVal n) -> (TInt, subst)
  Const (BoolVal n) -> (TBool, subst)
  e1 :+: e2 -> unify_bin_op e1 e2 TInt tenv subst
  e1 :*: e2 -> unify_bin_op e1 e2 TInt tenv subst
  --e1 :==: e2 -> e1
  -- if
  -- Var
  -- Let


unify_bin_op e1 e2 ty tenv subst =
  (ty, subst2)
  where subst2 = unifier t2 ty subst2' e2
        (t2, subst2') = type_of e2 tenv subst1
        subst1 = unifier t1 ty subst1' e1
        (t1, subst1') = type_of e1 tenv subst

-- type_of_program


---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------- Tests ----------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------

test_apply_one_subst1 =
  assert result TInt "test_apply_one_subst1"
  where result = apply_one_subst TInt (TVar 1) TBool

test_apply_one_subst2 =
  assert result TBool "test_apply_one_subst2"
  where result = apply_one_subst (TVar 1) (TVar 1) TBool

test_apply_one_subst3 =
  assert result (TVar 1) "test_apply_one_subst3"
  where result = apply_one_subst (TVar 1) (TVar 2) TBool

test_apply_one_subst4 =
  assert result (TArr TBool TBool) "test_apply_one_subst4"
  where result = apply_one_subst (TArr (TVar 1) (TVar 1)) (TVar 1) TBool

test_apply_one_subst5 =
  assert result (TArr (TVar 2) TBool) "test_apply_one_subst5"
  where result = apply_one_subst (TArr (TVar 2) (TVar 1)) (TVar 1) TBool



test_apply_subst_to_type1 =
  assert result (TArr (TVar 2) (TVar 1)) "test_apply_subst_to_type1"
  where result = apply_subst_to_type (TArr (TVar 2) (TVar 1)) Map.empty

test_apply_subst_to_type2 =
  assert result (TArr (TVar 2) TBool) "test_apply_subst_to_type2"
  where result = apply_subst_to_type (TArr (TVar 2) (TVar 1)) subst
        subst  = Map.insert 1 TBool Map.empty

test_apply_subst_to_type3 =
  assert result (TArr TInt TBool) "test_apply_subst_to_type3"
  where result = apply_subst_to_type (TArr (TVar 2) (TVar 1)) subst
        subst  = Map.insert 1 TBool (Map.insert 2 TInt Map.empty)

test_apply_subst_to_type4 =
  assert result (TArr (TArr TInt TBool) TInt) "test_apply_subst_to_type4"
  where result = apply_subst_to_type (TArr (TVar 1) TInt) subst
        subst  = Map.insert 1 (TArr TInt TBool) Map.empty



test_empty_subst =
  assert (Map.null empty_subst) True "test_empty_subst"

test_extend_subst1 =
  assert result new_subst "test_extend_subst1"
  where result = extend_subst old_subst (TVar 1) (TArr (TVar 2) TInt)
        old_subst = empty_subst
        new_subst  = Map.insert 1 (TArr (TVar 2) TInt) old_subst

test_extend_subst2 =
  assert result new_subst "test_extend_subst2"
  where result = extend_subst old_subst (TVar 1) (TArr (TVar 2) TInt)
        old_subst = Map.insert 3 (TArr (TVar 1) TInt) empty_subst
        new_subst  = Map.fromList [(1, (TArr (TVar 2) TInt)), (3, (TArr (TArr (TVar 2) TInt) TInt))]



test_no_occurrence1 =
  assert result True "test_no_occurrence1"
  where result = no_occurrence (TVar 1) (TArr (TVar 2) TInt)

test_no_occurrence2 =
  assert result True "test_no_occurrence2"
  where result = no_occurrence (TVar 1) (TArr TInt (TVar 2))

test_no_occurrence3 =
  assert result False "test_no_occurrence3"
  where result = no_occurrence (TVar 1) (TArr (TVar 1) TInt)

test_no_occurrence4 =
  assert result False "test_no_occurrence4"
  where result = no_occurrence (TVar 1) (TArr TInt (TVar 1))

test_no_occurrence5 =
  assert result True "test_no_occurrence5"
  where result = no_occurrence (TVar 1) TInt

test_no_occurrence6 =
  assert result True "test_no_occurrence6"
  where result = no_occurrence (TVar 1) TBool

test_no_occurrence7 =
  assert result False "test_no_occurrence7"
  where result = no_occurrence (TVar 1) (TVar 1)

test_no_occurrence8 =
  assert result True "test_no_occurrence8"
  where result = no_occurrence (TVar 1) (TVar 2)



test_show1 =
  assert result "bool" "test_show1"
  where result = show TBool

test_show2 =
  assert result "int" "test_show2"
  where result = show TInt

test_show3 =
  assert result "t1" "test_show3"
  where result = show (TVar 1)

test_show4 =
  assert result "int -> bool" "test_show4"
  where result = show (TArr TInt TBool)

test_show5 =
  assert result "int -> t1 -> t1" "test_show5"
  where result = show (TArr TInt (TArr (TVar 1) (TVar 1)))

test_standardize_type1 =
  assert result (TArr (TVar 1) (TArr (TVar 2) (TVar 1))) "test_standardize_type1"
  where result = standardize_type (TArr (TVar 10) (TArr (TVar 3) (TVar 10)))

test_standardize_type2 =
  assert result (TArr (TVar 1) (TArr (TVar 2) (TVar 3))) "test_standardize_type2"
  where result = standardize_type (TArr (TVar 10) (TArr (TVar 3) (TVar 13)))



test_unifier = True
-- OK type_of (Const (IntVal 3) :+: Const (IntVal 3)) EmptyTEnv empty_subst
-- NOT OK type_of (Const (IntVal 3) :+: Const (BoolVal True)) EmptyTEnv empty_subst

---------------------------------------------------------------------
-- I know about all the places in code where patterns ar not exhaustive
-- but if you get there by any chance I don't care what error is printed
-- as long as you are prevented from doing it eg.
--
-- `no_occurrence TInt TInt`
-- Is invalid because the first param should be a TVar
-- always for it to make sense
---------------------------------------------------------------------


testAll =
  if (allPassed)
    then "All tests passed."
    else error "Failed tests."
  where allPassed = test_apply_one_subst1 &&
                    test_apply_one_subst2 &&
                    test_apply_one_subst3 &&
                    test_apply_one_subst4 &&
                    test_apply_one_subst5 &&
                    test_apply_subst_to_type1 &&
                    test_apply_subst_to_type2 &&
                    test_apply_subst_to_type3 &&
                    test_apply_subst_to_type4 &&
                    test_empty_subst &&
                    test_extend_subst1 &&
                    test_extend_subst2 &&
                    test_no_occurrence1 &&
                    test_no_occurrence2 &&
                    test_no_occurrence3 &&
                    test_no_occurrence4 &&
                    test_no_occurrence5 &&
                    test_no_occurrence6 &&
                    test_no_occurrence7 &&
                    test_no_occurrence8 &&
                    test_show1 &&
                    test_show2 &&
                    test_show3 &&
                    test_show4 &&
                    test_show5 &&
                    test_standardize_type1 &&
                    test_standardize_type2


assert expected received message =
  if (expected == received)
    then True
    else error $ message ++ " -> expected: `" ++ (show expected) ++ "`; received: `" ++ (show received) ++ "`"
