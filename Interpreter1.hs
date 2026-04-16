module Interpreter(Stmt(..), executeStmt) where

import Common
import Memory
import Expression

data Stmt = StmtSkip
          | StmtAssign VariableName Expr
          | StmtSequence Stmt Stmt
          | StmtIf Expr Stmt Stmt
          | StmtWhile Expr Stmt

-- Example
factorial :: Stmt
factorial =
  StmtSequence
    (StmtAssign "r" (ExprConst (VInteger 1)))
    (StmtWhile
      -- Condition:
      (ExprBinary OpLessThan (ExprConst (VInteger 0)) (ExprVar "n"))
      -- Body:
      (StmtSequence
        (StmtAssign "r" (ExprBinary OpMultiply (ExprVar "r") (ExprVar "n")))
        (StmtAssign "n" (ExprBinary OpAdd (ExprVar "n") (ExprConst (VInteger (-1)))))))

--------------------------------------------------------------------------------

-- Exercise 3.a

executeStmt :: Stmt -> Mem -> Mem
executeStmt = error "COMPLETE"

--------------------------------------------------------------------------------

-- Exercise 3.b

twoToThePowerOfN :: Stmt
twoToThePowerOfN = error "COMPLETE"

--------------------------------------------------------------------------------

-- Exercise 3.c

-- Write at least three more tests.

test0 :: Bool
test0 = loadMem "r" finalMem == expectedValue
  where
    initialMem = storeMem "n" (VInteger 7) emptyMem
    finalMem   = executeStmt twoToThePowerOfN initialMem
    expectedValue = VInteger 128

test1 :: Bool
test1 = error "COMPLETE"

test2 :: Bool
test2 = error "COMPLETE"

test3 :: Bool
test3 = error "COMPLETE"

