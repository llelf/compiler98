module Prelude(Num(..)) where

#if !defined(TRACING)

import PrimIntegerAdd
import PrimIntegerSub
import PrimIntegerMul
import PrimIntegerNeg

instance Num Integer where
 a + b    = primIntegerAdd a b 
 a - b    = primIntegerSub a b 
 a * b    = primIntegerMul a b 
 negate a = primIntegerNeg a

 abs i = if i < 0 then negate i else i
 signum i = case compare i 0 of
		LT -> negate 1
		EQ ->  0
		GT ->  1
 fromInteger a = a -- id a

#else

instance Num Integer where
 a + b    = _prim _tprim_IntegerPlus a b
 a - b    = _prim _tprim_IntegerMinus a b
 a * b    = _prim _tprim_IntegerTimes a b
 negate a = _prim _tprim_IntegerNegate a

 abs i = if i < 0 then negate i else i
 signum i = case compare i 0 of
		LT -> -1
		EQ ->  0
		GT ->  1
 fromInteger a = a -- id a

_tprim_IntegerPlus primitive 3 :: Trace -> R Integer -> R Integer -> R Integer
_tprim_IntegerMinus primitive 3 :: Trace -> R Integer -> R Integer -> R Integer
_tprim_IntegerTimes primitive 3 :: Trace -> R Integer -> R Integer -> R Integer
_tprim_IntegerNegate primitive 2 :: Trace -> R Integer -> R Integer


#endif
