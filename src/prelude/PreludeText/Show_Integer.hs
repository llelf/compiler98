module Prelude (Show(..)) where

import Numeric(showSigned,showInt)

instance Show Integer where
  showsPrec = showSigned showInt
  showsType a = showString "Integer"

#if 0
-- earlier version for tracing
instance Show Integer where
  showsPrec p x = showString "<<Integer>>"
  showsType a = showString "Integer"

#endif
