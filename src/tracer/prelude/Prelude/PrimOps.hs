module Prelude(R(..), Trace, _prim,
               primIntEq, primIntNEq,
	       primIntPlus, primIntMinus, primIntTimes,
	       primIntAbs, primIntNegate, primIntSignum,
	       dbgIntFromInteger,
	       primIntQuot, primIntRem,
	       primIntLT, primIntLE, primIntGE, primIntGT,
	       primCharToEnum, primCharFromEnum,
	       prim_fromInteger, prim_IntFromInteger, 
	       prim_fromEnum,
	       primCHGetChar, primCHPutChar
	       ) where

--import DPrelude
import PreludeBuiltin(Handle)

-- Eq Int
primIntEq primitive 3 :: Trace -> R Int -> R Int -> R Bool
primIntNEq primitive 3 :: Trace -> R Int -> R Int -> R Bool

-- Num Int
primIntPlus primitive 3 :: Trace -> R Int -> R Int -> R Int
primIntMinus primitive 3 :: Trace -> R Int -> R Int -> R Int
primIntTimes primitive 3 :: Trace -> R Int -> R Int -> R Int
primIntAbs primitive 2 :: Trace -> R Int -> R Int
primIntNegate primitive 2 :: Trace -> R Int -> R Int
primIntSignum primitive 2 :: Trace -> R Int -> R Int
dbgIntFromInteger primitive 2 :: Trace -> R Integer -> R Int

-- Integral Int
primIntQuot primitive 3 :: Trace -> R Int -> R Int -> R Int
primIntRem primitive 3 :: Trace -> R Int -> R Int -> R Int

-- Ord Int
primIntLT primitive 3 :: Trace -> R Int -> R Int -> R Bool
primIntLE primitive 3 :: Trace -> R Int -> R Int -> R Bool
primIntGE primitive 3 :: Trace -> R Int -> R Int -> R Bool
primIntGT primitive 3 :: Trace -> R Int -> R Int -> R Bool

-- Enum Char
primCharToEnum primitive 2 :: Trace -> R Int -> R Char
primCharFromEnum primitive 2 :: Trace -> R Char -> R Int

-- Num
prim_fromInteger primitive 1 :: Integer -> a

-- Num Int
prim_IntFromInteger primitive 1 :: Integer -> Int

-- _FromEnum (needed when deriving Eq amongst other classes)

prim_fromEnum primitive 2 :: Trace -> R a -> R Int

--

primCHGetChar primitive 2 :: Trace -> R Handle -> R Int
primCHPutChar primitive 3 :: Trace -> R Handle -> R Char -> R (Either IOError ())
