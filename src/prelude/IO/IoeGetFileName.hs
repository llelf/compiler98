module IO where

import IO
import CString
import DIOError

ioeGetFileName        :: IOError -> Maybe FilePath
ioeGetFileName (IOErrorOpen file mode errno)     = Just (fromCString file)
ioeGetFileName ioerror = case ioeGetHandle ioerror of
			   Nothing -> Nothing
			   Just handle -> hGetFileName handle
