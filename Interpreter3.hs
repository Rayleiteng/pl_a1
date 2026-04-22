module Interpreter (Stmt (..), executeStmtWithReturn) where

import Common
import Expression
import Memory

data Stmt
  = StmtSkip
  | StmtAssign VariableName Expr
  | StmtSequence Stmt Stmt
  | StmtIf Expr Stmt Stmt
  | StmtWhile Expr Stmt
  | -- New
    StmtReturn Expr

data Result
  = RContinue Mem
  | RReturn Value
  deriving (Show, Eq)

--------------------------------------------------------------------------------

-- Exercise 5.a

executeStmtWithReturn :: Stmt -> Mem -> Result
executeStmtWithReturn stmt mem = case stmt of
  StmtSkip -> RContinue mem
  StmtAssign name expr -> RContinue (storeMem name (evalExpr mem expr) mem)
  StmtReturn expr -> RReturn (evalExpr mem expr)
  StmtSequence stmtone stmttwo -> case executeStmtWithReturn stmtone mem of
    RReturn val -> RReturn val
    RContinue mem1 -> executeStmtWithReturn stmttwo mem1
  StmtIf expr stmtone stmttwo ->
    if evalExpr mem expr == VBool True
      then executeStmtWithReturn stmtone mem
      else executeStmtWithReturn stmttwo mem
  StmtWhile expr stmt ->
    if evalExpr mem expr == VBool True
      then case executeStmtWithReturn stmt mem of
        RReturn val -> RReturn val
        RContinue mem1->executeStmtWithReturn (StmtWhile expr stmt) mem1
      else RContinue mem

--------------------------------------------------------------------------------

-- Exercise 5.b

squareRoot :: Stmt
squareRoot =
  StmtSequence
    (StmtAssign "i" (ExprConst (VInteger 0)))
    ( StmtWhile
        -- Condition:
        (ExprConst (VBool True))
        -- Body:
        ( StmtIf
          --Condition:
          (ExprUnary OpNot (ExprBinary OpLessThan (ExprBinary OpMultiply (ExprVar "i") (ExprVar "i")) (ExprVar "n")))
          -- Branch1
          (StmtReturn (ExprVar "i"))
          -- Branch2
          (StmtAssign "i" (ExprBinary OpAdd (ExprVar "i") (ExprConst (VInteger 1))))
        )
    )

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
test1 = executeStmtWithReturn stmt emptyMem == expectedResult
  where
    stmt = StmtSequence
             (StmtReturn (ExprConst (VInteger 1)))
             (StmtAssign "x" (ExprConst (VInteger 99)))
    expectedResult = RReturn (VInteger 1)

test2 :: Bool
test2 = executeStmtWithReturn stmt emptyMem == expectedResult
  where
    stmt = StmtSequence
             (StmtAssign "x" (ExprConst (VInteger 42)))
             (StmtReturn (ExprVar "x"))
    expectedResult = RReturn (VInteger 42)

test3 :: Bool
test3 = executeStmtWithReturn stmt emptyMem == expectedResult
  where
    cond = ExprBinary OpLessThan (ExprConst (VInteger 1)) (ExprConst (VInteger 2))
    stmt = StmtIf cond
             (StmtReturn (ExprConst (VInteger 1)))
             (StmtReturn (ExprConst (VInteger 2)))
    expectedResult = RReturn (VInteger 1)

test4 :: Bool
test4 = executeStmtWithReturn stmt emptyMem == expectedResult
  where
    cond = ExprBinary OpLessThan (ExprConst (VInteger 1)) (ExprConst (VInteger 2))
    stmt = StmtIf (ExprUnary OpNot cond)
             (StmtReturn (ExprConst (VInteger 1)))
             (StmtReturn (ExprConst (VInteger 2)))
    expectedResult = RReturn (VInteger 2)