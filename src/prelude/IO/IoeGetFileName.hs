module IO (ioeGetFileName) where

import DHandle
import DIOError
import HGetFileName
import IoeGetHandle

ioeGetFileName        :: IOError -> Maybe FilePath
ioeGetFileName (IOError cmd file@(Just _) _        errno)   = file

-- #if !defined(TRACING)
#if 1
ioeGetFileName ioerror = case ioeGetHandle ioerror of
			   Nothing -> Nothing
			   Just h -> hGetFileName h
#else
ioeGetFileName ioerror = case ioeGetHandle ioerror of
			   Nothing -> Nothing
			   Just h -> hGetFileName h
#endif
