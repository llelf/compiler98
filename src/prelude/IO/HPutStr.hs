module IO (hPutStr) where

import DHandle
import HPutChar

#if 0
-- previously #if !defined(TRACING)
-- but in some cases (typically the compiler itself), this is a big
-- space loss.
import GreenCard (toCString,PackedString)

foreign import hPutStrC :: Handle -> PackedString -> IO ()

hPutStr               :: Handle -> String -> IO ()
hPutStr h []           = return ()
hPutStr h s@(_:_)      = hPutStrC h (toCString s)

#else
hPutStr               :: Handle -> String -> IO ()
hPutStr h []           = return ()
hPutStr h (x:xs)       = hPutChar h x >> hPutStr h xs

#endif

