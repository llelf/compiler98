module IO(Eq(..)) where

import PrimEqHandlePosn
import DHandlePosn

instance Eq HandlePosn where
  a ==  b = primEqHandlePosn a b
