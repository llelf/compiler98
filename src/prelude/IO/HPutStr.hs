module IO (hPutStr) where

import DHandle
import HPutChar

hPutStr               :: Handle -> String -> IO ()
hPutStr h []          = return ()
hPutStr h (x:xs)      = hPutChar h x >> hPutStr h xs
