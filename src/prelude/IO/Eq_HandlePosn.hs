module IO(Eq(..)) where

import DHandlePosn
import FFI

#if !defined(TRACING)
foreign import primEqHandlePosnC :: HandlePosn -> HandlePosn -> Bool

instance Eq HandlePosn where
  a ==  b = primEqHandlePosnC a b

#else
foreign import primEqHandlePosnC :: ForeignObj -> ForeignObj -> Bool

instance Eq HandlePosn where
  (HandlePosn a) ==  (HandlePosn b) = primEqHandlePosnC a b
#endif
