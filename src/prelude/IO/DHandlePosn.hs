module IO(HandlePosn(..)) where

import FFI (ForeignObj)
import DHandle

data HandlePosn = HandlePosn Handle ForeignObj
