module IO where

import DIO
import PreludeBuiltin(Handle)
import HPutChar

hPutStr               :: Handle -> String -> IO ()
hPutStr h []          = return ()
hPutStr h (x:xs)      = hPutChar h x >> hPutStr h xs
