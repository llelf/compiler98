module System where

import FFI

foreign import primGetProgName :: IO CString

getProgName             :: IO String
getProgName = primGetProgName >>= return . fromCString

