module Prelude where

import IO

putChar :: Char -> IO ()
putChar c = hPutChar stdout c
