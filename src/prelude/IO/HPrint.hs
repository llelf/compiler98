module IO where

import IO

hPrint                :: Show a => Handle -> a -> IO ()
hPrint h a             = hPutStr h (show a)
