module IO (hGetPosn) where

import DHandle
import DHandlePosn
import FFI
import DIOError
import HGetFileName

#if !defined(TRACING)
foreign import hGetPosnC :: Handle -> Either Int Addr

hGetPosn              :: Handle -> IO HandlePosn
hGetPosn h = do
    a <- _mkIOwf1 (IOErrorC "hGetPosn" (hGetFileName h) . toEnum) hGetPosnC h
    f <- makeForeignObj a (free a)
    return (HandlePosn f)

#else
foreign import hGetPosnC :: ForeignObj -> Either Int Addr

hGetPosn              :: Handle -> IO HandlePosn
hGetPosn (Handle h) = do
    a <- _mkIOwf1 (IOErrorC "hGetPosn" (hGetFileName h) . toEnum) hGetPosnC h
    f <- makeForeignObj a (free a)
    return (HandlePosn f)

#endif
