module System where

import FFI

foreign import ccall primGetProgName :: IO PackedString

getProgName             :: IO String
getProgName = primGetProgName >>= return . fromCString

