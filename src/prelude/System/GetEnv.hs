module System where

import FFI

foreign import ccall primGetEnv   :: PackedString -> IO Addr
foreign import cast  addrToString :: Addr -> PackedString

getEnv                    :: String -> IO String
getEnv symbol = do
    ptr <- primGetEnv (toCString symbol)
    if ptr==nullAddr then do
        errno <- getErrNo
        throwIOError ("getEnv \""++symbol++"\"") Nothing Nothing errno
      else do
        return (fromCString (addrToString ptr))

