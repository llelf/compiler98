module Prelude(Enum(..)) where

instance Enum Integer where
  toEnum = toInteger
  fromEnum = fromInteger

  enumFrom = numericEnumFrom
  enumFromThen = numericEnumFromThen
