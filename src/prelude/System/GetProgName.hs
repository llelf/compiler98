module System where

import FFI

foreign import primGetProgName :: IO PackedString

getProgName             :: IO String
getProgName = primGetProgName >>= return . fromCString

