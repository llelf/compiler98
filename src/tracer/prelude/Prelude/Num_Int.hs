module Prelude(Num(..)) where

instance Num Int where
 a + b    = _prim _tprim_IntPlus a b
 a - b    = _prim _tprim_IntMinus a b
 a * b    = _prim _tprim_IntTimes a b

 negate a = _prim _tprim_IntNegate a
 abs    a = _prim _tprim_IntAbs a
 signum a = _prim _tprim_IntSignum a

 fromInteger i = _prim _tprim_IntFromInteger i

_tprim_IntPlus primitive 3 :: Trace -> R Int -> R Int -> R Int
_tprim_IntMinus primitive 3 :: Trace -> R Int -> R Int -> R Int
_tprim_IntTimes primitive 3 :: Trace -> R Int -> R Int -> R Int
_tprim_IntNegate primitive 2 :: Trace -> R Int -> R Int
_tprim_IntAbs primitive 2 :: Trace -> R Int -> R Int
_tprim_IntSignum primitive 2 :: Trace -> R Int -> R Int
_tprim_IntFromInteger primitive 2 :: Trace -> R Integer -> R Int
