module IO (hGetFileName) where

import DHandle
import NHC.FFI

-- #if !defined(TRACING)
#if 1
foreign import ccall hGetFileNameC :: Handle -> PackedString
hGetFileName h = Just (fromCString (hGetFileNameC h))

#else
foreign import ccall hGetFileNameC :: ForeignObj -> PackedString
hGetFileName (Handle h) = Just (fromCString (hGetFileNameC h))

#endif
