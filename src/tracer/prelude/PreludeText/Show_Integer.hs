module Prelude(Show(..)) where

import Text(showSigned,showInt)

instance Show Integer where
  showsPrec = showSigned showInt

  showsType a = showString "Integer"
