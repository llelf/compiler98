module IO (openFile) where

import DHandle
import IOMode
import DIOError
import FFI

foreign import openFileC :: CString -> Int -> Either Int ForeignObj

openFile              :: FilePath -> IOMode -> IO Handle
openFile fp iomode = do
    f <- _mkIOwf2 (IOErrorC ("openFile "++show iomode) (Just fp) . toEnum)
             openFileC (toCString fp) (fromEnum iomode)
    return (Handle f)

