module IO (Handle(..),ForeignObj) where

import FFIBuiltin (ForeignObj)

newtype Handle = Handle ForeignObj
