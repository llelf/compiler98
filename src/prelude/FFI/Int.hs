module FFI
  -- all types are abstract and instances of:
  -- Num, Bounded, Real, Integral, Ix, Enum, Read, Show
  ( Int8
  , Int16
  , Int32
  , Int64
  ) where

import FFIBuiltin (Int8, Int16, Int32, Int64)

instance Num Int8 where

