module Prelude (Show(..)) where

#if !defined(TRACING)

import Numeric(showSigned,showFloat)

instance Show Float where
  showsPrec = showSigned showFloat
  showsType a = showString "Float"

#else

instance Show Float where
  showsPrec p x = showString "<<Float>>"
  showsType a = showString "Float"

#endif

