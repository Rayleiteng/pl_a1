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
showMem mem = "{" ++ (plugin . sort) mem ++ "}"

sort :: Mem -> Mem
sort (MkMem []) = MkMem []
sort (MkMem ((name, val) : ns)) =
  if val /= VInteger 0
    then insert (name, val) (sort (MkMem ns))
    else sort (MkMem ns)
  where
    insert (name, val) (MkMem []) = MkMem [(name, val)]
    insert (name, val) (MkMem ((n, v) : ns)) =
      if name < n
        then MkMem ((name, val) : (n, v) : ns)
        else MkMem ((n, v) : r)
      where
        MkMem r = insert (name, val) (MkMem ns)

plugin :: Mem -> String
plugin (MkMem []) = ""
plugin (MkMem [(name, val)]) =
  name
    ++ " -> "
    ++ case val of
      VInteger v -> show v
      VBool v -> show v
plugin (MkMem ((name, val) : ns)) =
  name
    ++ " -> "
    ++ case val of
      VInteger v -> show v
      VBool v -> show v
    ++ ","
    ++ plugin (MkMem ns)

--------------------------------------------------------------------------------

-- Exercise 1.e
eqMem :: Mem -> Mem -> Bool
eqMem m1 m2 = eq (sort m1) (sort m2)

eq :: Mem -> Mem -> Bool
eq (MkMem []) (MkMem []) = True
eq (MkMem []) _ = False
eq _ (MkMem []) = False
eq (MkMem ((n1, v1) : ns1)) (MkMem ((n2, v2) : ns2)) = (n1 == n2) && (v1 == v2) && eq (MkMem ns1) (MkMem ns2)


test1 :: Mem
test1 =
  storeMem
    "a"
    (VInteger 2)
    ( storeMem
        "c"
        (VInteger 10)
        ( storeMem
            "b"
            (VBool True)
            ( storeMem "a" (VInteger 1) emptyMem
            )
        )
    )

test2 :: Mem
test2 = emptyMem
