module IO(Eq(..)) where

import DHandle
import FFI


foreign import primEqHandleC     :: ForeignObj -> ForeignObj -> Bool

instance Eq Handle where
  (Handle h)  == (Handle j)   =   primEqHandleC h j
