module Prelude where

infixr 0 `seq`

seq         :: a -> b -> b
seq a b      = seq a b      -- MAGIC
