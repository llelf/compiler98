module FFI 
  ( getErrNo
  , mkIOError
  , throwIOError
  ) where

import DErrNo
import DIOError
import IO (Handle)

foreign import getErrNo :: IO Int

mkIOError :: String -> Maybe FilePath -> Maybe Handle -> Int -> IOError
mkIOError str mf mh err = IOErrorC str mf (toEnum err)

throwIOError :: String -> Maybe FilePath -> Maybe Handle -> Int -> IO a
throwIOError str mf mh err = ioError (mkIOError str mf mh err)
