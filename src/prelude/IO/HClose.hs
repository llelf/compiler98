module IO (hClose,hCloseC) where

import DHandle
import NHC.FFI

foreign import ccall hCloseC :: ForeignObj -> IO ()

hClose                :: Handle -> IO () 
#if !defined(TRACING)
hClose (Handle f) = freeForeignObj f
#else
hClose (Handle f) = hCloseC f
#endif
