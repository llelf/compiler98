module IO where

import DIOError

isUserError  :: IOError -> Maybe String
isUserError (IOErrorUser str) = Just str
isUserError ioerror = Nothing
