module IO(Eq(..)) where

import DHandle
import FFI


foreign import ccall primEqHandleC     :: ForeignObj -> ForeignObj -> Bool

instance Eq Handle where
  (Handle h)  == (Handle j)   =   primEqHandleC h j
