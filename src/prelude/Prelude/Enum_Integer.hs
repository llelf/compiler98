module Prelude(Enum(..)) where

instance Enum Integer where
  toEnum x = toInteger x
  fromEnum x = fromInteger x

  enumFrom x = numericEnumFrom x
  enumFromThen x y = numericEnumFromThen x y
