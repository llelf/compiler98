module Prelude where


-- primIntegerQuot primitive 2 :: Integer -> Integer -> Integer
-- primIntegerRem primitive 2 :: Integer -> Integer -> Integer

foreign import "primIntegerQuotC" primIntegerQuot :: Integer -> Integer -> Integer
foreign import "primIntegerRemC" primIntegerRem :: Integer -> Integer -> Integer


primIntegerQuotRem a b = (primIntegerQuot a b,primIntegerRem a b)
