module Array(Array(..),Vector) where

import PreludeBuiltin(Vector)

data  (Ix a)    => Array a b = MkArray (a,a) (Vector b)
