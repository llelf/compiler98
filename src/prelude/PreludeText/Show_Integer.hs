module Prelude (Show(..)) where

#if !defined(TRACING)

import Numeric(showSigned,showInt)

instance Show Integer where
  showsPrec = showSigned showInt

  showsType a = showString "Integer"

#else

instance Show Integer where
  showsPrec p x = showString "<<Integer>>"
  showsType a = showString "Integer"

#endif
