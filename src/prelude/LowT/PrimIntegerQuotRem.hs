module Prelude where


foreign import "primIntegerQuotC" primIntegerQuot :: Integer -> Integer -> Integer
foreign import "primIntegerRemC" primIntegerRem :: Integer -> Integer -> Integer

primIntegerQuotRem a b = (primIntegerQuot a b,primIntegerRem a b)
