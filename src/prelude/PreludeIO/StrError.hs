module Prelude (strError) where

import FFI
import DErrNo

foreign import ccall "strerror" primStrError :: Int -> PackedString

strError :: ErrNo -> String
strError e = fromCString (primStrError (fromEnum e))
