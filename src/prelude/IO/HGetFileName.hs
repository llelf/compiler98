module IO (hGetFileName) where

import DHandle
import FFI

#if !defined(TRACING)
foreign import hGetFileNameC :: Handle -> PackedString
hGetFileName h = Just (fromCString (hGetFileNameC h))

#else
foreign import hGetFileNameC :: ForeignObj -> PackedString
hGetFileName (Handle h) = Just (fromCString (hGetFileNameC h))

#endif
