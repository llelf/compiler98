module Prelude where

infixr 0 `seq`

class Eval a where
   seq         :: a -> b -> b
   strict      :: (a -> b) -> a -> b

   seq a b = seq a b      -- MAGIC
   strict f x  =  x `seq` f x
