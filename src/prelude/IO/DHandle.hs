module IO (Handle(..),ForeignObj) where

import FFI (ForeignObj)

newtype Handle = Handle ForeignObj
