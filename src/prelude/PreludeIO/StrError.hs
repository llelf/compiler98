module Prelude (strError) where

import FFI
import DErrNo

foreign import "strerror" primStrError :: Int -> CString

strError :: ErrNo -> String
strError e = fromCString (primStrError (fromEnum e))
