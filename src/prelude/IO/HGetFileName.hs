module IO (hGetFileName) where

import DHandle
import FFI

#if !defined(TRACING)
foreign import hGetFileNameC :: Handle -> CString
hGetFileName h = Just (fromCString (hGetFileNameC h))

#else
foreign import hGetFileNameC :: ForeignObj -> CString
hGetFileName h = Just (fromCString (hGetFileNameC h))

#endif
