module IO(Eq(..)) where

import DHandlePosn
import DHandle
import Eq_Handle
import FFI


foreign import ccall primEqHandlePosnC :: ForeignObj -> ForeignObj -> Bool

instance Eq HandlePosn where
  (HandlePosn h a) == (HandlePosn j b)   =   h==j  &&  primEqHandlePosnC a b
