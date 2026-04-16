module Interpreter(Stmt(..), executeStmtWithReturn) where

import Common
import Memory
import Expression

data Stmt = StmtSkip
          | StmtAssign VariableName Expr
          | StmtSequence Stmt Stmt
          | StmtIf Expr Stmt Stmt
          | StmtWhile Expr Stmt
          -- New
          | StmtReturn Expr

data Result = RContinue Mem
            | RReturn Value
  deriving (Show, Eq)

--------------------------------------------------------------------------------

-- Exercise 5.a

executeStmtWithReturn :: Stmt -> Mem -> Result
executeStmtWithReturn = error "COMPLETE"

--------------------------------------------------------------------------------

-- Exercise 5.b

squareRoot :: Stmt
squareRoot = error "COMPLETE"

--------------------------------------------------------------------------------
--
-- Exercise 5.c

-- Write at least three more tests.

test0 :: Bool
test0 = executeStmtWithReturn squareRoot initialMem == expectedResult
  where
    initialMem = storeMem "n" (VInteger 99) emptyMem
    expectedResult = RReturn (VInteger 10)

test1 :: Bool
test1 = error "COMPLETE"

test2 :: Bool
test2 = error "COMPLETE"

test3 :: Bool
test3 = error "COMPLETE"

