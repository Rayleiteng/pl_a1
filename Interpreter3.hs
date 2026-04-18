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

--------------------------------------------------------------------------------
-- Test 1: 验证 Sequence 语句的短路特性 (s1 执行了 return，s2 必须被跳过)
-- 对应文档要求："if s1 returns a value then s2 is not executed"
--------------------------------------------------------------------------------
test1 :: Bool
test1 = executeStmtWithReturn stmt emptyMem == expectedResult
  where
    -- 程序逻辑: return(1); x := 99
    stmt = StmtSequence
             (StmtReturn (ExprConst (VInteger 1)))
             (StmtAssign "x" (ExprConst (VInteger 99)))
    -- 预期结果: 解释器应当立即返回 1，彻底无视后面的赋值语句
    expectedResult = RReturn (VInteger 1)

--------------------------------------------------------------------------------
-- Test 2: 验证 Sequence 语句的正常流转 (s1 不 return，s2 执行并 return)
-- 对应文档要求："if s1 does not return a value then s2 must be executed and it can return a value"
--------------------------------------------------------------------------------
test2 :: Bool
test2 = executeStmtWithReturn stmt emptyMem == expectedResult
  where
    -- 程序逻辑: x := 42; return(x)
    stmt = StmtSequence
             (StmtAssign "x" (ExprConst (VInteger 42)))
             (StmtReturn (ExprVar "x"))
    -- 预期结果: 前半部分改变了内存，后半部分成功读取内存并返回 42
    expectedResult = RReturn (VInteger 42)

--------------------------------------------------------------------------------
-- Test 3: 验证 If 语句分支隔离 (条件为真，屏蔽 else 分支的 return)
-- 对应文档要求："if the condition is true then the return statements in s2 have no effect"
--------------------------------------------------------------------------------
test3 :: Bool
test3 = executeStmtWithReturn stmt emptyMem == expectedResult
  where
    -- 程序逻辑: if (1 < 2) { return(1) } else { return(2) }
    cond = ExprBinary OpLessThan (ExprConst (VInteger 1)) (ExprConst (VInteger 2))
    stmt = StmtIf cond
             (StmtReturn (ExprConst (VInteger 1))) -- True 分支
             (StmtReturn (ExprConst (VInteger 2))) -- False 分支
    -- 预期结果: 因为 1 < 2 恒为真，所以只能返回 1，else 分支里的 2 绝对不能生效
    expectedResult = RReturn (VInteger 1)