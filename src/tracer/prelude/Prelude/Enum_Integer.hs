module Prelude(Enum(..)) where

import NumericEnumFrom
import NumericEnumFromThen

instance Enum Integer where
  toEnum = toInteger
  fromEnum = fromInteger

  enumFrom = numericEnumFrom
  enumFromThen = numericEnumFromThen
