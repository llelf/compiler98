module IO (isEOFError) where

import DIOError

isEOFError            :: IOError -> Bool
isEOFError (IOErrorEOF fun file) = True
isEOFError ioerror = False
