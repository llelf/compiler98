module Prelude(Enum(..)) where

instance Enum Char where
    toEnum   c = toEnum c	-- MAGIC
    fromEnum c = fromEnum c	-- MAGIC

    enumFrom n        = enumFromTo n (toEnum 255)
    enumFromThen n n' = enumFromThenTo n n' (toEnum 255)
