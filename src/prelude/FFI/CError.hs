module FFI 
  ( getErrNo
  , mkIOError
  ) where

import DErrNo
import DIOError
import IO (Handle)

foreign import getErrNo :: IO Int

mkIOError :: String -> Maybe FilePath -> Maybe Handle -> Int -> IO a
mkIOError str mf mh err = ioError (IOErrorC str mf (toEnum err))

