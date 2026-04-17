module Common(VariableName, Value(..)) where

type VariableName = String

data Value = VInteger Integer
           | VBool Bool
  deriving (Show, Eq)
