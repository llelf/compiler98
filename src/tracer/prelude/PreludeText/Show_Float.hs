module Prelude(Show(..)) where

import Text(showSigned,showFloat)

instance Show Float where
  showsPrec = showSigned showFloat
  showsType a = showString "Float"
