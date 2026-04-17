module Interpreter (Stmt (..), executeStmt) where

import Common
import Expression
import Memory

data Stmt
  = StmtSkip
  | StmtAssign VariableName Expr
  | StmtSequence Stmt Stmt
  | StmtIf Expr Stmt Stmt
  | StmtWhile Expr Stmt

-- Example
factorial :: Stmt
factorial =
  StmtSequence
    (StmtAssign "r" (ExprConst (VInteger 1)))
    ( StmtWhile
        -- Condition:
        (ExprBinary OpLessThan (ExprConst (VInteger 0)) (ExprVar "n"))
        -- Body:
        ( StmtSequence
            (StmtAssign "r" (ExprBinary OpMultiply (ExprVar "r") (ExprVar "n")))
            (StmtAssign "n" (ExprBinary OpAdd (ExprVar "n") (ExprConst (VInteger (-1)))))
        )
    )

--------------------------------------------------------------------------------

-- Exercise 3.a

executeStmt :: Stmt -> Mem -> Mem
executeStmt stmt mem = case stmt of
  StmtSkip -> mem
  StmtAssign name expr -> storeMem name (evalExpr mem expr) mem
  StmtSequence stmtone stmttwo -> executeStmt stmttwo (executeStmt stmtone mem)
  StmtIf expr stmtone stmttwo ->
    if evalExpr mem expr == VBool True
      then executeStmt stmtone mem
      else executeStmt stmttwo mem
  StmtWhile expr stmt ->
    if evalExpr mem expr == VBool True
      then executeStmt (StmtWhile expr stmt) (executeStmt stmt mem)
      else mem

--------------------------------------------------------------------------------

-- Exercise 3.b

twoToThePowerOfN :: Stmt
twoToThePowerOfN =
  StmtSequence
    (StmtAssign "r" (ExprConst (VInteger 1)))
    ( StmtWhile
        -- Condition:
        (ExprBinary OpLessThan (ExprConst (VInteger 0)) (ExprVar "n"))
        -- Body:
        ( StmtSequence
            (StmtAssign "r" (ExprBinary OpMultiply (ExprVar "r") (ExprConst (VInteger 2))))
            (StmtAssign "n" (ExprBinary OpAdd (ExprVar "n") (ExprConst (VInteger (-1)))))
        )
    )

--------------------------------------------------------------------------------

-- Exercise 3.c

-- Write at least three more tests.

test0 :: Bool
test0 = loadMem "r" finalMem == expectedValue
  where
    initialMem = storeMem "n" (VInteger 7) emptyMem
    finalMem = executeStmt twoToThePowerOfN initialMem
    expectedValue = VInteger 128

test1 :: Bool
test1 = loadMem "r" finalMem == VInteger 1
  where
    initialMem = storeMem "n" (VInteger 0) emptyMem
    finalMem = executeStmt twoToThePowerOfN initialMem

test2 :: Bool
test2 = loadMem "r" finalMem == VInteger 8
  where
    initialMem = storeMem "n" (VInteger 3) emptyMem
    finalMem = executeStmt twoToThePowerOfN initialMem

test3 :: Bool
test3 = loadMem "r" finalMem == VInteger 32
  where
    initialMem = storeMem "n" (VInteger 5) emptyMem
    finalMem = executeStmt twoToThePowerOfN initialMem
