module Prelude(Num(..)) where

--import PrimIntegerAdd
--import PrimIntegerSub
--import PrimIntegerMul
--import PrimIntegerNeg

instance Num Integer where
 a + b    = _prim _tprim_IntegerPlus a b -- primIntegerAdd a b 
 a - b    = _prim _tprim_IntegerMinus a b -- primIntegerSub a b 
 a * b    = _prim _tprim_IntegerTimes a b -- primIntegerMul a b 
 negate a = _prim _tprim_IntegerNegate a -- primIntegerNeg a

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

