module Prelude(Num(..)) where

--import PrimFloatFromInteger

instance Num Float where
 a + b    = _prim _tprim_FloatPlus a b -- a + b       -- MAGIC
 a - b    = _prim _tprim_FloatMinus a b -- a - b       -- MAGIC
 a * b    = _prim _tprim_FloatTimes a b -- a * b       -- MAGIC
 
 negate a = _prim _tprim_FloatNegate a -- negate a    -- MAGIC
 abs    a = _prim _tprim_FloatAbs a -- abs    a    -- MAGIC
 signum a = _prim _tprim_FloatSignum a -- signum a    -- MAGIC
 
 fromInteger i = _prim _tprim_FloatFromInteger i

_tprim_FloatPlus primitive 3 :: Trace -> R Float -> R Float -> R Float
_tprim_FloatMinus primitive 3 :: Trace -> R Float -> R Float -> R Float
_tprim_FloatTimes primitive 3 :: Trace -> R Float -> R Float -> R Float

_tprim_FloatNegate primitive 2 :: Trace -> R Float -> R Float
_tprim_FloatAbs primitive 2 :: Trace -> R Float -> R Float
_tprim_FloatSignum primitive 2 :: Trace -> R Float -> R Float

_tprim_FloatFromInteger primitive 2 :: Trace -> R Integer -> R Float
