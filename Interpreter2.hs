module Interpreter (Stmt (..), executeStmtWithLog) where

import Common
import Expression
import Memory

data Stmt
  = StmtSkip
  | StmtAssign VariableName Expr
  | StmtSequence Stmt Stmt
  | StmtIf Expr Stmt Stmt
  | StmtWhile Expr Stmt
  | -- New:
    StmtWrite Expr

--------------------------------------------------------------------------------

-- Exercise 4.a

type OutputLog = [Value]

executeStmtWithLog :: Stmt -> Mem -> (OutputLog, Mem)
executeStmtWithLog stmt mem = case stmt of
  StmtSkip -> ([], mem)
  StmtAssign name expr -> ([], storeMem name (evalExpr mem expr) mem)
  StmtWrite expr -> ([evalExpr mem expr], mem)
  StmtSequence stmtone stmttwo -> (log1 ++ log2, mem2)
    where
      (log1, mem1) = executeStmtWithLog stmtone mem
      (log2, mem2) = executeStmtWithLog stmttwo mem1
  StmtIf expr stmtone stmttwo ->
    if evalExpr mem expr == VBool True
      then executeStmtWithLog stmtone mem
      else executeStmtWithLog stmttwo mem
  StmtWhile expr stmt ->
    if evalExpr mem expr == VBool True
      then (log1 ++ log2, mem2)
      else ([], mem)
    where
      (log1, mem1) = executeStmtWithLog stmt mem
      (log2, mem2) = executeStmtWithLog (StmtWhile expr stmt) mem1

--------------------------------------------------------------------------------

-- Exercise 4.b

countToN :: Stmt
countToN =
  StmtSequence
    (StmtAssign "i" (ExprConst (VInteger 0)))
    ( StmtWhile
        -- Condition:
        (ExprBinary OpLessThan (ExprVar "i") (ExprVar "n"))
        -- Body:
        ( StmtSequence
            (StmtWrite (ExprVar "i"))
            (StmtAssign "i" (ExprBinary OpAdd (ExprVar "i") (ExprConst (VInteger 1))))
        )
    )

--------------------------------------------------------------------------------

-- Exercise 4.c

-- Write at least three more tests.

test0 :: Bool
test0 = finalOutputLog == expectedValue
  where
    initialMem = storeMem "n" (VInteger 5) emptyMem
    finalOutputLog = fst (executeStmtWithLog countToN initialMem)
    expectedValue = [VInteger 0, VInteger 1, VInteger 2, VInteger 3, VInteger 4]

test1 :: Bool
test1 = finalOutputLog == expectedValue
  where
    stmt =
      StmtSequence
        (StmtWrite (ExprConst (VInteger 1)))
        ( StmtSequence
            (StmtWrite (ExprConst (VInteger 2)))
            (StmtWrite (ExprConst (VInteger 3)))
        )
    finalOutputLog = fst (executeStmtWithLog stmt emptyMem)
    expectedValue = [VInteger 1, VInteger 2, VInteger 3]

test2 :: Bool
test2 = finalOutputLog == expectedValue
  where
    cond = ExprBinary OpLessThan (ExprConst (VInteger 1)) (ExprConst (VInteger 2))
    stmt =
      StmtIf
        cond
        (StmtWrite (ExprConst (VInteger 1)))
        (StmtWrite (ExprConst (VInteger 0)))

    finalOutputLog = fst (executeStmtWithLog stmt emptyMem)
    expectedValue = [VInteger 1]

test3 :: Bool
test3 = finalOutputLog == expectedValue
  where
    cond = ExprBinary OpLessThan (ExprConst (VInteger 2)) (ExprConst (VInteger 1))
    stmt =
      StmtSequence
        ( StmtIf
            cond
            (StmtWrite (ExprConst (VInteger 1)))
            (StmtWrite (ExprConst (VInteger 0)))
        )
        ( StmtIf
            (ExprUnary OpNot cond)
            (StmtWrite (ExprConst (VInteger 1)))
            (StmtWrite (ExprConst (VInteger 0)))
        )

    finalOutputLog = fst (executeStmtWithLog stmt emptyMem)
    expectedValue = [VInteger 0, VInteger 1]