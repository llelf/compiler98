module IO(Eq(..)) where

import DHandlePosn
import DHandle
import FFI


foreign import primEqHandlePosnC :: ForeignObj -> ForeignObj -> Bool
foreign import primEqHandleC     :: ForeignObj -> ForeignObj -> Bool

instance Eq HandlePosn where
  (HandlePosn (Handle h) a) == (HandlePosn (Handle j) b) =
		primEqHandleC h j  &&  primEqHandlePosnC a b
