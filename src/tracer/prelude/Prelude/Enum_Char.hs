module Prelude(Enum(..)) where

instance Enum Char where
    toEnum   c = _prim _tprim_CharToEnum c
    fromEnum c = _prim _tprim_CharFromEnum c

{-
    enumFrom n        = enumFromTo n (toEnum 255)
    enumFromThen n n' = enumFromThenTo n n' (toEnum 255)
-}

_tprim_CharToEnum primitive 2 :: Trace -> R Int -> R Char
_tprim_CharFromEnum primitive 2 :: Trace -> R Char -> R Int
