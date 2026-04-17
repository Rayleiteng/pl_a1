module Memory (Mem, emptyMem, loadMem, storeMem, showMem) where

import Common

data Mem = MkMem [(VariableName, Value)]

instance Show Mem where
  show = showMem

instance Eq Mem where
  (==) = eqMem

--------------------------------------------------------------------------------

-- Exercise 1.a
emptyMem :: Mem
emptyMem = error "COMPLETE"

--------------------------------------------------------------------------------

-- Exercise 1.b
loadMem :: VariableName -> Mem -> Value
loadMem name (MkMem []) = VInteger 0
loadMem name (MkMem ((n, x) : ms)) =
  if name == n
    then x
    else loadMem name (MkMem ms)

--------------------------------------------------------------------------------

-- Exercise 1.c
storeMem :: VariableName -> Value -> Mem -> Mem
storeMem = error "COMPLETE"

--------------------------------------------------------------------------------

-- Exercise 1.d
showMem :: Mem -> String
showMem = error "COMPLETE"

--------------------------------------------------------------------------------

-- Exercise 1.e
eqMem :: Mem -> Mem -> Bool
eqMem = error "COMPLETE"
