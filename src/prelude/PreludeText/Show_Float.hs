module Prelude (Show(..)) where

#if 1

import Numeric(showSigned,showFloat)

instance Show Float where
  showsPrec = showSigned showFloat
  showsType a = showString "Float"

#else

instance Show Float where
  showsPrec p x = showString "<<Float>>"
  showsType a = showString "Float"

#endif

