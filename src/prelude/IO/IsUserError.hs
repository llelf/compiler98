module IO where

import DIOError

isUserError  :: IOError -> Bool
isUserError (IOErrorUser str) = True
isUserError ioerror = False
