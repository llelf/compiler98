module IO where

import IO

hPutStrLn      :: Handle -> String -> IO ()
hPutStrLn h s   =  do hPutStr h s
                      hPutChar h '\n'

