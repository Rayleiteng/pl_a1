{- HLINT ignore "Use newtype instead of data" -}
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
emptyMem = MkMem []

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
storeMem name val (MkMem []) = MkMem [(name, val)]
storeMem name val (MkMem ((n, x) : ms)) =
  if name == n
    then MkMem ((n, val) : ms)
    else MkMem ((n, x) : ms')
    where
      MkMem ms' = storeMem name val (MkMem ms)

--------------------------------------------------------------------------------

-- Exercise 1.d
showMem :: Mem -> String
showMem = error "COMPLETE"

--------------------------------------------------------------------------------

-- Exercise 1.e
eqMem :: Mem -> Mem -> Bool
eqMem = error "COMPLETE"
