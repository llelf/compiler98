module IO where

import IO

hPutStr               :: Handle -> String -> IO ()
hPutStr h []          = return ()
hPutStr h (x:xs)      = hPutChar h x >> hPutStr h xs
