module IO(HandlePosn(..)) where

import FFI (ForeignObj)

newtype HandlePosn = HandlePosn ForeignObj
