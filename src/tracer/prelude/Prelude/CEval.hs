module Prelude(Eval(..)) where

infixr 0 `seq`

class Eval a where
   seq         :: a -> b -> b
   strict      :: (a -> b) -> a -> b

   seq a b = _prim _tprim_seq a b -- seq a b      -- MAGIC
   strict f x  =  x `seq` f x

_tprim_seq primitive 3 :: Trace -> R a -> R b -> R b
