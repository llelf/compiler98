module IO(HandlePosn(..)) where

import FFI (ForeignPtr)
import DHandle

data HandlePosn = HandlePosn Handle (ForeignPtr ())
