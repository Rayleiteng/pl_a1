module Expression (OpBinary (..), OpUnary (..), Expr (..), foldExpr, evalExpr) where

import Common
import Data.List (nub)
import Memory

data OpBinary
  = OpAdd -- (e1 + e2)
  | OpMultiply -- (e1 * e2)
  | OpAnd -- (e1 && e2)
  | OpOr -- (e1 || e2)
  | OpEqual -- (e1 == e2)
  | OpLessThan -- (e1 < e2)
  deriving (Show)

data OpUnary
  = OpMinus -- (- e)
  | OpNot -- (! e)
  deriving (Show)

data Expr
  = ExprVar VariableName
  | ExprConst Value
  | ExprBinary OpBinary Expr Expr
  | ExprUnary OpUnary Expr
  deriving (Show)

--------------------------------------------------------------------------------

-- Exercise 2.a

-- foldExpr :: ...
foldExpr :: (VariableName -> a) -> (Value -> a) -> (OpBinary -> a -> a -> a) -> (OpUnary -> a -> a) -> Expr -> a
foldExpr fVar fConst fBinary fUnary expr = case expr of
  ExprVar name -> fVar name
  ExprConst value -> fConst value
  ExprBinary op e1 e2 -> fBinary op (rec e1) (rec e2)
  ExprUnary op e -> fUnary op (rec e)
  where
    rec = foldExpr fVar fConst fBinary fUnary

--------------------------------------------------------------------------------

-- Exercise 2.b
variablesExpr :: Expr -> [VariableName]
variablesExpr = foldExpr (\s -> [s]) (const []) (\_ p q -> nub (p ++ q)) (\_ p -> p)

--------------------------------------------------------------------------------

-- Exercise 2.c
renameExpr :: Expr -> VariableName -> VariableName -> Expr
renameExpr expr old new =
  foldExpr
    ( \name ->
        if name == old
          then ExprVar new
          else
            if name == new
              then ExprVar old
              else ExprVar name
    )
    ExprConst
    ExprBinary
    ExprUnary
    expr

--------------------------------------------------------------------------------

-- Exercise 2.d
evalExpr :: Mem -> Expr -> Value
evalExpr mem = foldExpr (fVar mem) fConst fBinary fUnary
  where
    fVar :: Mem -> VariableName -> Value
    fVar mem name = loadMem name mem
    fConst :: Value -> Value
    fConst = id
    fBinary :: OpBinary -> Value -> Value -> Value
    fBinary op v1 v2 = case (op, v1, v2) of
      (OpAdd, VInteger a, VInteger b) -> VInteger (a + b)
      (OpMultiply, VInteger a, VInteger b) -> VInteger (a * b)
      (OpAnd, VBool a, VBool b) -> VBool (a && b)
      (OpOr, VBool a, VBool b) -> VBool (a || b)
      (OpEqual, _, _) -> VBool (v1 == v2)
      (OpLessThan, VInteger a, VInteger b) -> VBool (a < b)
    fUnary :: OpUnary -> Value -> Value
    fUnary op v = case (op, v) of
      (OpMinus, VInteger a) -> VInteger (-a)
      (OpNot, VBool a) -> VBool (not a)

--------------------------------------------------------------------------------

-- Exercise 2.e
--
-- Write at least three more tests.
-- The tests should return True in case of success.

test0 :: Bool
test0 = evalExpr mem expr == expectedValue
  where
    mem = storeMem "x" (VInteger 1) emptyMem
    expr = ExprVar "x"
    expectedValue = VInteger 1

test1 :: Bool
test1 = evalExpr mem expr == expectedValue
  where
    mem = storeMem "x" (VInteger 2) emptyMem
    expr = ExprBinary OpMultiply (ExprConst (VInteger 3)) (ExprVar "x")
    expectedValue = VInteger 6

test2 :: Bool
test2 = evalExpr mem expr == expectedValue
  where
    mem = storeMem "x" (VInteger 2) emptyMem
    expr =
      ExprBinary
        OpEqual
        (ExprBinary OpMultiply (ExprConst (VInteger 3)) (ExprVar "x"))
        (ExprConst (VInteger 6))
    expectedValue = VBool True

test3 :: Bool
test3 = evalExpr mem expr == expectedValue
  where
    mem = emptyMem
    expr = ExprBinary OpAdd (ExprConst (VBool False)) (ExprConst (VInteger 3))
    expectedValue = VInteger 0

main :: IO ()
main = do
  print test1
  print test2
  print test3
