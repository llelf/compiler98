module IO (ioeGetFileName) where

import CString
import DHandle
import DIOError
import HGetFileName
import IoeGetHandle

ioeGetFileName        :: IOError -> Maybe FilePath
ioeGetFileName (IOErrorOpen file mode errno)     = Just (fromCString file)
ioeGetFileName (IOErrorC cmd maybefile errno)    = maybefile

#if !defined(TRACING)
ioeGetFileName ioerror = case ioeGetHandle ioerror of
			   Nothing -> Nothing
			   Just h -> hGetFileName h
#else
ioeGetFileName ioerror = case ioeGetHandle ioerror of
			   Nothing -> Nothing
			   Just (Handle h) -> hGetFileName h
#endif
