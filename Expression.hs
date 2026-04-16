module Expression(OpBinary(..), OpUnary(..), Expr(..), foldExpr, evalExpr) where

import Common
import Memory

data OpBinary = OpAdd      -- (e1 + e2)
              | OpMultiply -- (e1 * e2)
              | OpAnd      -- (e1 && e2)
              | OpOr       -- (e1 || e2)
              | OpEqual    -- (e1 == e2)
              | OpLessThan -- (e1 < e2)
  deriving Show

data OpUnary = OpMinus  -- (- e)
             | OpNot    -- (! e)
  deriving Show

data Expr = ExprVar VariableName
          | ExprConst Value
          | ExprBinary OpBinary Expr Expr
          | ExprUnary OpUnary Expr
  deriving Show

--------------------------------------------------------------------------------

-- Exercise 2.a

foldExpr :: (VariableName -> b) 
         -> (Value -> b) 
         -> (OpBinary -> b -> b -> b) 
         -> (OpUnary -> b -> b) 
         -> Expr -> b
foldExpr cVar cConst cBinary cUnary expr =
  case expr of
    ExprVar x     -> cVar x
    ExprConst x   -> cConst x
    ExprBinary op e1 e2 -> cBinary op (rec e1) (rec e2)
    ExprUnary op e1 -> cUnary op (rec e1)
  where
    rec = foldExpr cVar cConst cBinary cUnary

--------------------------------------------------------------------------------

-- Exercise 2.b
variablesExpr :: Expr -> [VariableName]
variablesExpr = error "COMPLETE"

--------------------------------------------------------------------------------

-- Exercise 2.c
renameExpr :: Expr -> [VariableName]
renameExpr = error "COMPLETE"

--------------------------------------------------------------------------------

-- Exercise 2.d
evalExpr :: Mem -> Expr -> Value
evalExpr = error "COMPLETE"

--------------------------------------------------------------------------------

-- Exercise 2.e

-- Write at least three more tests.
-- The tests should return True in case of success.

test0 :: Bool
test0 = evalExpr mem expr == expectedValue
  where
    mem = storeMem "x" (VInteger 1) emptyMem
    expr = ExprVar "x"
    expectedValue = VInteger 1

test1 :: Bool
test1 = error "COMPLETE"

test2 :: Bool
test2 = error "COMPLETE"

test3 :: Bool
test3 = error "COMPLETE"

