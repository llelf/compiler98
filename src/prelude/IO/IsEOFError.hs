module IO where

import IO
import DIOError

isEOFError            :: IOError -> Bool
isEOFError (IOErrorEOF fun file) = True
isEOFError ioerror = False
