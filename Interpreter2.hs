module Interpreter(Stmt(..), executeStmtWithLog) where

import Common
import Memory
import Expression

data Stmt = StmtSkip
          | StmtAssign VariableName Expr
          | StmtSequence Stmt Stmt
          | StmtIf Expr Stmt Stmt
          | StmtWhile Expr Stmt
          -- New:
          | StmtWrite Expr

--------------------------------------------------------------------------------

-- Exercise 4.a

type OutputLog = [Value]

executeStmtWithLog :: Stmt -> Mem -> (OutputLog, Mem)
executeStmtWithLog = error "COMPLETE"

--------------------------------------------------------------------------------

-- Exercise 4.b

countToN :: Stmt
countToN = error "COMPLETE"

--------------------------------------------------------------------------------

-- Exercise 4.c

-- Write at least three more tests.

test0 :: Bool
test0 = finalOutputLog == expectedValue
  where
    initialMem = storeMem "n" (VInteger 5) emptyMem
    finalOutputLog  = fst (executeStmtWithLog countToN initialMem)
    expectedValue = [VInteger 0, VInteger 1, VInteger 2, VInteger 3, VInteger 4]

test1 :: Bool
test1 = error "COMPLETE"

test2 :: Bool
test2 = error "COMPLETE"

test3 :: Bool
test3 = error "COMPLETE"

