module IO (hSetPosn) where

import DHandle
import DHandlePosn
import DIOError
import HGetFileName

#if !defined(TRACING)

foreign import hSetPosnC :: Handle -> HandlePosn -> Either Int ()

hSetPosn              :: Handle -> HandlePosn -> IO () 
hSetPosn h p = _mkIOwf2 (IOErrorC "hSetPosn" (hGetFileName h) . toEnum)
                   hSetPosnC h p

#else

foreign import hSetPosnC :: ForeignObj -> ForeignObj -> Either Int ()

hSetPosn              :: Handle -> HandlePosn -> IO () 
hSetPosn (Handle h) (HandlePosn p) =
       _mkIOwf2 (IOErrorC "hSetPosn" (hGetFileName h) . toEnum) hSetPosnC h p

#endif
