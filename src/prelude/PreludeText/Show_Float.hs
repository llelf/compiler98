module Prelude (Show(..)) where

#if 1

import Numeric(showFloat)

instance Show Float where
  showsPrec p = showFloat
  showsType a = showString "Float"

#else

instance Show Float where
  showsPrec p x = showString "<<Float>>"
  showsType a = showString "Float"

#endif

